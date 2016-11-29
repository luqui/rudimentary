{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}

module Levels where

import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Control.Monad.Random as Rand

import Syntax
import qualified Params as P

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


type Params = P.Params String P.SelectWidget String

type Level = Params (Dist (Exp, String -> Bool))
data Sublevel = Sublevel { 
    slName :: String,
    slDesc :: String,
    slLevel :: Level }

chooseLevels :: String -> [Sublevel] -> Level
chooseLevels l = join . P.select l l . map toTuple 
    where
    toTuple (Sublevel name desc level) = (name, desc, level)

levels :: Level
levels = chooseLevels "Level" [
    majorScaleIntervals, majorScaleIntervalsOct ]
    

majorScaleIntervals :: Sublevel
majorScaleIntervals = Sublevel {
    slName = "Major scale intervals",
    slDesc = concat ["I will play the 1 and then another degree of the major scale, ",
                        "and you need to identify which degree that is as a number."],
    slLevel = return $ do
        i <- uniform [1..7]
        return (Exp (Scale (Note 0) (Mode Natural 7)) (DERun [Degree 0 0, Degree i 0]), 
                (== show (i + 1)))
  }
 

majorScaleIntervalsOct :: Sublevel
majorScaleIntervalsOct = Sublevel {
    slName = "Major scale intervals (with octave differences)",
    slDesc = concat ["I will play the 1 and then another degree of the major scale, ",
                        "and you need to identify which degree that is as a number. Ignore ",
                        "octave differences, so if I play a C and then a D <i>below</i> it, the ",
                        "answer is 2."],
    slLevel = return $ do
        i <- uniform [-14..14]
        refoct <- uniform [-14,-7,0,7,14]
        return (Exp (Scale (Note 0) (Mode Natural 7)) (DERun [Degree refoct 0, Degree i 0]), 
                (== show ((i `mod` 7) + 1)))
  }
