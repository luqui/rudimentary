{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}

module Levels where

import Control.Applicative
import Control.Monad (join)
import qualified Control.Monad.Random as Rand

import Syntax

type Dist = Rand.Rand Rand.StdGen

dist :: [(Double, a)] -> Dist a
dist choices = do
    x <- Rand.getRandomR (0, sum (map fst choices))
    let choose _ [] = error "no choices"
        choose accum ((p,a):as)
            | x <= accum + p  = return a
            | otherwise       = choose (accum+p) as
    choose 0 choices

uniform :: [a] -> Dist a
uniform = dist . map (1,)

data Level = Level {
    levelName :: String,
    levelDesc :: String,
    levelSpec :: Dist (Exp, String -> Bool) }

levels :: [Level]
levels = [
    majorScaleIntervals,
    majorScaleIntervalsOct ]

majorScaleIntervals :: Level
majorScaleIntervals = Level {
    levelName = "Major scale intervals",
    levelDesc = concat ["I will play the 1 and then another degree of the major scale, ",
                        "and you need to identify which degree that is as a number."],
    levelSpec = do
        i <- uniform [1..7]
        return (Exp (Scale (Note 0) (Mode Natural 7)) (DERun [Degree 0 0, Degree i 0]), 
                (== show ((i `mod` 7) + 1)))
  }


majorScaleIntervalsOct :: Level
majorScaleIntervalsOct = Level {
    levelName = "Major scale intervals (with octave differences)",
    levelDesc = concat ["I will play the 1 and then another degree of the major scale, ",
                        "and you need to identify which degree that is as a number. Ignore ",
                        "octave differences, so if I play a C and then a D <i>below</i> it, the ",
                        "answer is 2."],
    levelSpec = do
        i <- uniform [-14..14]
        refoct <- uniform [-14,-7,0,7,14]
        return (Exp (Scale (Note 0) (Mode Natural 7)) (DERun [Degree refoct 0, Degree i 0]), 
                (== show ((i `mod` 7) + 1)))
  }
