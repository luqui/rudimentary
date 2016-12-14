{-# LANGUAGE DeriveFunctor #-}

module Semantics where

import Control.Applicative
import Data.List (sort)

import Syntax

data Media a
    = Prim a
    | Media a :+: Media a
    | Media a :=: Media a
    deriving (Functor, Show)

-- This is only well-defined when every Prim takes the same amount of time
instants :: Media a -> [[a]]
instants (Prim x) = [[x]]
instants (a :+: b) = instants a ++ instants b
instants (a :=: b) = zipm (instants a) (instants b)
    where
    zipm [] ys = ys
    zipm xs [] = xs
    zipm (x:xs) (y:ys) = (x `mappend` y) : zipm xs ys

instance (Ord a) => Eq (Media a) where
    a == b = map sort (instants a) == map sort (instants b)

-- The applicative instance is multiplication.
instance Applicative Media where
    pure = Prim
    Prim f <*> m = fmap f m
    (a :+: b) <*> m = (a <*> m) :+: (b <*> m)
    (a :=: b) <*> m = (a <*> m) :=: (b <*> m)

retrograde :: Media a -> Media a
retrograde (Prim a) = Prim a
retrograde (a :+: b) = retrograde b :+: retrograde a
retrograde (a :=: b) = retrograde a :=: retrograde b

modeIntervals :: Mode -> [Int]
modeIntervals (Mode g m) = trunc intervals (drop (m-1) (cycle (genusIntervals g)))
    where
    intervals = genusIntervals g
    trunc = zipWith (const id)

renderScale :: Scale -> [Int]
renderScale (Scale (Note note0) scaleType) = init $ scanl (+) (60+note0) (modeIntervals scaleType)

shift :: Degree -> Degree -> Degree
shift Rest _ = Rest
shift _ Rest = Rest
shift (Degree a acca) (Degree b accb) = Degree (a+b) (acca+accb)

(<>) :: Media Degree -> Media Degree -> Media Degree
(<>) = liftA2 shift

invert :: Degree -> Degree
invert (Degree a acc) = Degree (negate a) (negate acc)
invert Rest = Rest

applyScale :: [Int] -> Degree -> Int
applyScale scale (Degree deg acc) = (scale !! (deg `mod` len)) + 12 * (deg `div` len) + acc
    where
    len = length scale
applyScale _ Rest = 0

interpDegreeExp :: DegreeExp -> Media Degree
interpDegreeExp = ($ Prim (Degree 0 0)) . go
    where
    go (DERun run) = (foldr1 (:+:) (map Prim run) <>)
    go (DEParallel f g) = liftA2 (:=:) (go f) (go g)
    go (DEMult f g) = go f . go g
    go (DEConcat f g) = liftA2 (:+:) (go f) (go g)
    go (DEOp DOIdentity) = id
    go (DEOp DOInvert) = fmap invert
    go (DEOp DORetrograde) = retrograde
    
evalExp :: Exp -> Media Int
evalExp (Exp scale degexp) = fmap (applyScale (renderScale scale)) (interpDegreeExp degexp)

