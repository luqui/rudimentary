{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, LambdaCase, TupleSections, ScopedTypeVariables #-}

module Main where

import Control.Monad (filterM, liftM, ap, forever, (>=>))
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import qualified System.MIDI as MIDI
import qualified Data.Sequence as Seq

import qualified StreamProc as S
import qualified Exercises

connectOutput :: String -> IO MIDI.Connection
connectOutput destName = do
    destinations <- MIDI.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> MIDI.getName d) destinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< MIDI.getName destination
    return conn

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
    source <- connectInput "UM-ONE"
    dest <- connectOutput "IAC Bus 1"
    MIDI.start source
    print =<< runStreamProc source dest (fromExerciseForm exercise)
    --print =<< runStreamProc source dest (fromExerciseForm playEx)

exercise :: S.StreamProc Exercises.Note Exercises.Note Exercises.SegDiff
exercise = do
    let target = Exercises.fourNoteArpeggioSegment (Exercises.baseScale 48 Exercises.majorScale)
    seg <- Exercises.metronomeIntro tempo 8 (Exercises.recordSegment tempo target)
    return $ Exercises.diffSegments target seg
    where
    tempo = 80

playEx :: S.StreamProc Exercises.Note Exercises.Note ()
playEx = Exercises.playSegment 80 $ 
    Exercises.fourNoteArpeggioSegment (Exercises.baseScale 48 Exercises.majorScale)

runStreamProc :: MIDI.Connection -> MIDI.Connection -> 
                 S.StreamProc MIDI.MidiMessage MIDI.MidiMessage a -> IO a
runStreamProc inDevice outDevice = \proc -> do
    startTime <- MIDI.currentTime inDevice
    go startTime Nothing proc
    where
    -- e is a one-message buffer
    go t e (S.Input (S.Return proc)) = go t e proc
    go t e (S.Input (S.Wait d f)) = loop e
        where
        loop e = do
            event <- maybe (MIDI.getNextEvent inDevice) (return . Just) e
            (timestamp, message) <- case event of
                Nothing -> fmap (,Nothing) (MIDI.currentTime inDevice)
                Just (MIDI.MidiEvent timestamp message) -> return (timestamp, Just message)
            let timediff = S.TimeDiff (fromIntegral (timestamp-t) / 1000)
            case message of
                Nothing
                    -- there might be skew here, since we're not accounting for t-timediff
                    | timediff >= d -> go timestamp Nothing (S.Input (f Nothing))
                    | otherwise -> threadDelay 1000 >> loop Nothing
                Just m
                    -- skew here too
                    | timediff >= d -> go timestamp e (S.Input (f Nothing))
                    | otherwise -> go timestamp Nothing (S.Input (f (Just (timediff, m))))
    go t e (S.Output m proc) = do
        MIDI.send outDevice m
        go t e proc
    go t e (S.Caboose x) = return x

fromExerciseForm :: S.StreamProc Exercises.Note Exercises.Note a 
                 -> S.StreamProc MIDI.MidiMessage MIDI.MidiMessage a
fromExerciseForm = S.mapFilterIO msgToNote noteToMsg
    where
    msgToNote (MIDI.MidiMessage _ (MIDI.NoteOn key vel))
        | vel /= 0 = Just (Exercises.NoteOn key vel)
        | vel == 0 = Just (Exercises.NoteOff key)
    msgToNote (MIDI.MidiMessage _ (MIDI.NoteOff key _))
        = Just (Exercises.NoteOff key)
    msgToNote _ = Nothing

    noteToMsg (Exercises.NoteOn key vel) = Just (MIDI.MidiMessage 1 (MIDI.NoteOn key vel))
    noteToMsg (Exercises.NoteOff key) = Just (MIDI.MidiMessage 1 (MIDI.NoteOn key 0))
