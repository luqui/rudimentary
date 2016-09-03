{-# LANGUAGE BangPatterns #-}

module Exercises where

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

metronome :: Double -> StreamProc i Int a
metronome tempo = go 0
    where
    go !n = do
        output n
        waitTime (TimeDiff (60/tempo))
        go (n+1)

randomMetronome :: Double -> StreamProc i Int a
randomMetronome = mapFilterO (guard <$> fst . Random.random . Random.mkStdGen <*> id) . metronome
    where
    guard False = const Nothing
    guard True = Just


