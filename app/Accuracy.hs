{-# LANGUAGE DeriveFunctor, TupleSections #-}

module Main where

import Control.Concurrent.STM

import Control.Arrow (second)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (ap, forever)
import MIDI (connectInput, connectOutput)

import qualified Data.Map as Map
import qualified System.MIDI as SysMid

type Time = Double
infinity :: Time
infinity = 1/0

class (Functor f) => Temporal f where
    withTime :: f a -> f (Time, a)


newtype Future a = Future { unFuture :: STM (Time, a) }
    deriving (Functor)

instance Monad Future where
    return = Future . return . (-infinity,)
    m >>= f = Future $ do
        (t, x) <- unFuture m
        (t', y) <- unFuture (f x)
        return (max t t', y)

instance Applicative Future where
    pure = return
    (<*>) = ap

instance Temporal Future where
    withTime (Future f) = Future $ do
        (t, x) <- f
        return (t, (t, x))

trySTM :: STM a -> STM (Maybe a)
trySTM a = (Just <$> a) `orElse` return Nothing

earliest :: Future a -> Future a -> Future a
earliest f f' = Future $ do
    v  <- trySTM (unFuture f)
    v' <- trySTM (unFuture f')
    case (v, v') of
        (Nothing,     Nothing) -> retry
        (Just (t, x), Nothing) -> return (t, x)
        (Nothing,     Just (t', x')) -> return (t', x')
        (Just (t, x), Just (t', x'))
            | t <= t'   -> return (t, x)
            | otherwise -> return (t', x')

runFuture :: Future a -> IO (Time, a)
runFuture = atomically . unFuture 


-- A sorted stream of Futures
newtype Event a = Event { unEvent :: Future (a, Event a) }
    deriving (Functor)

instance Temporal Event where
    withTime (Event e) = Event $ reassoc <$> withTime e
        where
        reassoc (t, (x, e')) = ((t, x), withTime e')

insert :: Future a -> Event a -> Event a
insert f e = Event $ earliest ((, e) <$> f) (second (insert f) <$> unEvent e)

filterE :: (a -> Bool) -> Event a -> Event a
filterE p e = Event $ do
    (x, e') <- unEvent e
    if p x
        then return (x, filterE p e')
        else unEvent (filterE p e')


data NoteEvent = NoteEvent Int Int (Future ()) 
type NoteStream = Event NoteEvent



main = do
    source <- connectInput "UM-ONE"
    dest <- connectOutput "IAC Bus 1"
    events <- inputEvents source
    playEvents dest events

newSinkFuture :: SysMid.Connection -> IO (a -> IO (), Future a)
newSinkFuture conn = do
    tvar <- atomically $ newTVar Nothing
    let fut = Future $ readTVar tvar >>= maybe retry return
    let sink x = do
            time <- (/1000) . fromIntegral <$> SysMid.currentTime conn
            atomically $ writeTVar tvar (Just (time, x))
    return (sink, fut)
        

inputLoop :: SysMid.Connection -> ((NoteEvent, NoteStream) -> IO ()) -> Map.Map Int (IO ()) -> IO ()
inputLoop conn sink onNotes = do
    maybeEv <- SysMid.getNextEvent conn
    let noteOn note vel = do
            (noteEndSink, noteEndFut) <- newSinkFuture conn
            (nextNoteSink, nextNoteFut) <- newSinkFuture conn
            sink (NoteEvent note vel noteEndFut, Event nextNoteFut)
            inputLoop conn nextNoteSink (Map.insert note (noteEndSink ()) onNotes)
    let noteOff note = do
            case Map.lookup note onNotes of
                Nothing -> inputLoop conn sink onNotes
                Just noteEndSink -> noteEndSink >> inputLoop conn sink (Map.delete note onNotes)
    case maybeEv of
        Nothing -> threadDelay 100 >> inputLoop conn sink onNotes
        Just (SysMid.MidiEvent _ (SysMid.MidiMessage _ch (SysMid.NoteOn note vel)))
            | vel == 0 -> noteOff note
            | otherwise -> noteOn note vel
        Just (SysMid.MidiEvent _ (SysMid.MidiMessage _ch (SysMid.NoteOff note _)))
            -> noteOff note
        _ -> inputLoop conn sink onNotes

inputEvents :: SysMid.Connection -> IO NoteStream
inputEvents conn = do
    (sink, fut) <- newSinkFuture conn
    forkIO $ inputLoop conn sink Map.empty
    return (Event fut)

outputLoop :: SysMid.Connection -> Event (Either NoteEvent Int) -> IO ()
outputLoop conn stream = do
    (_, (val, stream')) <- runFuture (unEvent stream)
    case val of
        Left (NoteEvent note vel noteEndFut) -> do
            SysMid.send conn (SysMid.MidiMessage 1 (SysMid.NoteOn note vel))
            outputLoop conn (insert (Right note <$ noteEndFut) stream')
        Right note -> do
            SysMid.send conn (SysMid.MidiMessage 1 (SysMid.NoteOff note 64))
            outputLoop conn stream'

playEvents :: SysMid.Connection -> NoteStream -> IO ()
playEvents conn stream = outputLoop conn (Left <$> stream)

