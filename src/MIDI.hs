module MIDI where

import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (filterM, when)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.MIDI as SysMid

import Semantics

connectOutput :: String -> IO SysMid.Connection
connectOutput destName = do
    destinations <- SysMid.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> SysMid.getName d) destinations
    conn <- SysMid.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< SysMid.getName destination
    return conn

connectInput :: String -> IO SysMid.Connection
connectInput srcName = do
    sources <- SysMid.enumerateSources
    [source] <- filterM (\d -> (srcName ==) <$> SysMid.getName d) sources
    conn <- SysMid.openSource source Nothing
    putStrLn . ("Connected to source " ++) =<< SysMid.getName source
    SysMid.start conn
    return conn

midiLock :: IO a -> IO a
midiLock = \action -> do takeMVar lock ; x <- action ; putMVar lock () ; return x
    where
    {-# NOINLINE lock #-}
    lock = unsafePerformIO $ newMVar ()

playNotes :: Double -> Media Int -> SysMid.Connection -> IO ()
playNotes dt (Prim note) conn = do
    when (note /= 0) . midiLock $ SysMid.send conn (SysMid.MidiMessage 1 (SysMid.NoteOn note 64))
    threadDelay (floor (10^6 * dt))
    when (note /= 0) . midiLock $ SysMid.send conn (SysMid.MidiMessage 1 (SysMid.NoteOff note 0))
playNotes dt (m :+: m') conn = do
    playNotes dt m conn
    playNotes dt m' conn
playNotes dt (m :=: m') conn = do
    v <- newEmptyMVar
    v' <- newEmptyMVar
    _ <- forkFinally (playNotes dt m conn) $ \_ -> putMVar v ()
    _ <- forkFinally (playNotes dt m' conn) $ \_ -> putMVar v' ()
    () <- takeMVar v
    () <- takeMVar v'
    return ()
    
