{-# LANGUAGE TupleSections #-}

module Levels where

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

levels :: [(String, Dist Exp)]
levels = [
    "Natural Modes of C" --> Exp <$> (Scale <$> pure (Note 0) <*> naturalMode) <*> asc,
    "Melodic Modes of C" --> Exp <$> (Scale <$> pure (Note 0) <*> melodicMode) <*> asc,
    "Modes of C"         --> 
        Exp <$> (Scale <$> pure (Note 0) <*> join (uniform [naturalMode, melodicMode])) <*> asc,

    "Intervals from C" -->
        Exp (Scale (Note 0) (Mode Natural 7)) <$> do {
            n <- uniform [1..7];
            acc <- uniform [-1..1]; 
            return (DEParallel (DERun [Degree 0 0]) (DERun [Degree n acc])) },


    "Chords of C major" --> Exp (Scale (Note 0) (Mode Natural 7)) <$> chord 3,
    "Chords of Natural Modes of C (3 notes)" -->  
        Exp <$> (Scale <$> pure (Note 0) <*> naturalMode) <*> chord 3,
    "Chords of Natural Modes of C (4 notes)" --> 
        Exp <$> (Scale <$> pure (Note 0) <*> naturalMode) <*> chord 4
    ]
    where
    infix 1 -->
    (-->) = (,)

    asc = pure (DERun (map (`Degree` 0) [0..7]))
    naturalMode = Mode <$> pure Natural <*> uniform [1..7]
    melodicMode = Mode <$> pure Melodic <*> uniform [1..7]

    chord :: Int -> Dist DegreeExp
    chord = fmap (foldr1 DEParallel . map (\n -> DERun [Degree n 0])) . go
        where
        go n = do
            n1 <- uniform [0..6]
            (n1:) <$> go' (n-1) n1
        go' 0 _ = return []
        go' n accum = do
            ni <- uniform [1..3]
            ((accum+ni) :) <$> go' (n-1) (accum+ni)
