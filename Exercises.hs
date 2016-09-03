{-# LANGUAGE BangPatterns #-}

module Exercises where

import Control.Monad (forever)
import StreamProc
import qualified System.Random as Random

data Note 
    = NoteOn Int Int
    | NoteOff Int
    deriving (Eq, Show)

majorScaleDegrees = [0,2,4,5,7,9,11]
scaleAt start degrees = 
    map (+start) degrees ++ [start + (degrees !! 0) + 12] ++ map (+start) (reverse degrees)

cScale :: StreamProc Note Note ()
cScale = go (scaleAt 60 majorScaleDegrees)
    where
    go [] = return ()
    go (n:ns) = do
        event <- waitForever
        case event of
            NoteOn pitch vel | pitch == n -> output (NoteOn pitch vel) >> go ns
            NoteOff pitch -> output (NoteOff pitch) >> go (n:ns)
            _ -> go (n:ns)

metronome :: Double -> StreamProc i () a
metronome tempo = forever $ do
    output ()
    waitTime (TimeDiff (60/tempo))

maskedMetronome :: Double -> [Bool] -> StreamProc i () a
maskedMetronome tempo bools = 
    mapFilterO maybify (scanO (\(b:bs) _ -> (b,bs)) bools (metronome tempo))
    where
    maybify False = Nothing
    maybify True = Just ()


