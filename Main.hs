{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (filterM, liftM, ap, forever, (>=>))
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import qualified System.MIDI as MIDI
import qualified Data.Sequence as Seq

{-
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
-}

-- Use cases:
-- - Playing scales/arpeggios to a metronome, measuring timing and accuracy.
-- -- Record and analyze, just need to trigger metronome.
-- -- Need to reset current scale if there's a two beat pause.
-- - Randomly generating keys and chords and measuring response and time.
-- -- Similar, with a more complex metronome. 
-- -- Possibly need to t

-- Here's an idea, we have a (causal) function EventStream -> EventStream.  The
-- resulting EventStream is what ends up getting played (can even filter out
-- source notes).  For metronome, we just merge with a metronome EventStream, and then
-- we can analyze afterward.  
--
-- It's dangerous to try to express it as a proper function; as reactive taught
-- us, we probably need a more robust FRP model.  We need to be able to merge,
-- we need to process events in context (e.g. looking at recent events), and
-- generate new ones.

{-
-- Possibly we need StreamProc to be a consumer and a producer simultaneously, sort of a stream
-- processor, sort of imperative.

detectReset :: StreamProc (Either Beat a) ()
detectReset = mapCX $ \cx -> \case
            Left beat -> when (null [ () | Right _ <- latest (2 * beatSize) cx ]) (gen ())
            Right _ -> return ()

currentScale :: Int -> Int -> StreamProc (Either Beat Note) Int
currentScale beats start = do
    gen start
    reset <- wait ((detectReset >> gen True) `merge` (waitTime (beats * beatSize) >> gen False))
    if reset
        then currentScale beats start
        else currentScale beats (start+1)
-}

newtype TimeDiff = TimeDiff Double
    deriving (Num)

data Future i a 
    = Wait TimeDiff (Maybe i -> Future i a)
    | Return a

instance Monad (Future i) where
    return = Return
    Return a >>= t = t a
    Wait d f >>= t = Wait d (f >=> t)

instance Functor (Future i) where fmap = liftM
instance Applicative (Future i) where pure = return; (<*>) = ap


data Train f o a 
    = Car (f (o, Train f o a))
    | Caboose a

instance (Functor f) => Monad (Train f o) where
    return = Caboose
    Caboose a >>= t = t a
    Car f >>= t = Car ((fmap.fmap) (>>= t) f)

instance (Functor f) => Functor (Train f o) where fmap = liftM
instance (Functor f) => Applicative (Train f o) where pure = return; (<*>) = ap


type StreamProc i o a = Train (Future i) o a



