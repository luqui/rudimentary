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
import qualified Semantics

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

type Level = Params (Dist (Exp, String -> Maybe Bool))
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
    intervals ]

select :: String -> [(String, String, a)] -> Params a
select title = P.select title title

select' :: String -> String -> [(String, a)] -> Params a
select' title desc options = select title [ (opt, desc, x) | (opt, x) <- options ]

intervals :: Sublevel
intervals = Sublevel {
    slName = "Interval Degrees",
    slDesc = concat ["I will play the 1 and then another degree of the scale, ",
                     "and you need to identify which degree that is as a number."],
    slLevel = do
        octRange <- select' 
            "Octave Range" 
            (concat ["How many octaves the interval spans.  Ignore octave differences ",
                     "in your answer, so if I play a C and then a D below it, the answer ",
                     "is still 2."])  
            [ (show r, r) | r <- [1..5] ]
        let minNote = -7 * (octRange `div` 2)
        let maxNote = 7 * ((octRange+1) `div` 2)

        keyDist <- select "Key"
            [ ("C", "The reference tone will always be the same, C", pure (Note 0))
            , ("Random", "The reference tone will be a random note", Note <$> uniform [0..11]) ]

        scale <- majorOrChromatic

        return $ do
            refNote <- uniform [minNote,minNote+7..maxNote-1]
            note <- uniform [minNote..maxNote]
            let (degDist, grade) = scale note
            deg <- degDist
            key <- keyDist
            return (Exp (Scale key (Mode Natural 7)) (DERun [Degree refNote 0, deg]), 
                    grade deg)
  }

majorOrChromatic :: Params (Int -> (Dist Degree, Degree -> String -> Maybe Bool))
majorOrChromatic = select "Scale"
        [ ("Major", "All intervals will be generated in the major scale, your answer " ++
                    "is just a number for degree of the scale.", majorDist)
        , ("Chromatic", "Arbitrary intervals will be generated.  Your answer will include " ++
                    "the interval and the quality, e.g. M7, m3, P5", chrDist) ]
    where
    majorDist deg = (pure (Degree deg 0), runParser parseSimpleInterval)

    chrDist deg = (Degree deg <$> accDist, runParser parseInterval)
        where
        accDist | any (deg `mod` 7 ==) [0,3,4] = uniform [-1,0,1]
                | otherwise = uniform [-2,-1,0,1]

    runParser p deg s = case parseString p s of
                            Left _ -> Nothing
                            Right x -> Just (compareDegrees deg x)
    compareDegrees i j = 
        Semantics.applyScale cMajor (norm i) == Semantics.applyScale cMajor (norm j)
    norm (Degree i acc) = Degree (i `mod` 7) acc
    cMajor = Semantics.renderScale (Scale (Note 0) (Mode Natural 7))
