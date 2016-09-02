module Exercises where

import StreamProc

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
