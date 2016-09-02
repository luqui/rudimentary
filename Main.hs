{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad (filterM, forever)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import qualified System.MIDI as MIDI
import qualified Data.Sequence as Seq

connectOutput :: String -> IO PlayState
connectOutput destName = do
    destinations <- MIDI.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> MIDI.getName d) destinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< MIDI.getName destination
    return $ PlayState {
        psConnection = conn,
        psPlayingNotes = [],
        psFreeChannels = Seq.fromList [1..8]
    }

connectInput :: String -> IO MIDI.Connection
connectInput sourceName = do
    sources <- MIDI.enumerateSources
    validSources <- filterM (\s -> (sourceName ==) <$> MIDI.getName s) sources
    putStrLn $ "There are " ++ show (length validSources) ++ " valid sources"
    let [source] = validSources
    conn <- MIDI.openSource source Nothing
    putStrLn . ("Connected to source " ++) =<< MIDI.getName source
    return conn

main :: IO ()
main = do
    state <- connectOutput "IAC Bus 1"
    source <- connectInput "UM-ONE"
    MIDI.start source
    (`evalStateT` state) . forever $ do
        liftIO $ threadDelay 1000  -- 1 millisec
        events <- liftIO (MIDI.getEvents source)
        mapM_ procEvent events

procEvent :: MIDI.MidiEvent -> StateT PlayState IO ()
procEvent (MIDI.MidiEvent _ (MIDI.MidiMessage _ msg)) = go msg
    where
    go (MIDI.NoteOn key vel) = noteOnKey key vel
    go (MIDI.NoteOff key _) = noteOffKey key
    go _ = return ()
