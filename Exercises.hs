{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Exercises where

import Control.Monad (forever)
import StreamProc
import qualified System.Random as Random

data Note 
    = NoteOn Int Int
    | NoteOff Int
    deriving (Eq, Show)


newtype Scale = Scale { degree :: Int -> Int }

makeScale :: [Int] -> Scale
makeScale xs = Scale $ \n -> 12 * (n `div` len) + xs !! (n `mod` len)
    where
    len = length xs

modalScale :: Scale -> Int -> Scale
modalScale s deg = Scale $ \n -> degree s (n+deg) - root
    where
    root = degree s deg

majorScale = makeScale [0,2,4,5,7,9,11]
naturalMinorScale = modalScale majorScale 5
harmonicMinorScale = makeScale [0,2,3,5,7,8,11]
melodicMinorScale = makeScale [0,2,3,5,7,9,11]
wholeToneScale = makeScale [0,2,4,6,8,10]
diminishedScale = makeScale [0,1,3,4,6,7,9,10]

baseScale :: Int -> Scale -> Scale
baseScale base s = Scale $ \n -> degree s n + base

diatonicShiftScale :: Int -> Scale -> Scale
diatonicShiftScale shift s = Scale $ \n -> degree s (n + shift)


-- An exercise is a collection of segments (e.g. there might be one for every key), meant
-- to be concatenated, but split into segments so that we can start from the latest segment
-- in case of errors, and to report accuracy for each segment separately.  The Doubles are
-- times (in beats) from the beginning of the segment.
newtype Segment = Segment [(Double,Int)]
    deriving Show
newtype Exercise = Exercise [Segment]
    deriving (Show, Monoid)

fourNoteArpeggioSegment :: Scale -> Segment
fourNoteArpeggioSegment s = Segment .  zip [0,1/4..] . map (degree s) $
        concat [ [ a, a+2, a+4, a+6 ] | a <- [0..7] ]
     ++ concat [ [ a+6, a+4, a+2, a ] | a <- reverse [0..7] ]

invertedFourNoteArpeggioSegment :: Scale -> Segment
invertedFourNoteArpeggioSegment s = Segment . zip [0,1/4..] . map (degree s) $
        concat [ [ a, a+2, a+4, a+6 ] | a <- reverse [0..7] ]
     ++ concat [ [ a+6, a+4, a+2, a ] | a <- [0..7] ]


type KeyTraversal = [Int]

chromaticKeyTraversal, fourthsKeyTraversal, fifthsKeyTraversal  :: KeyTraversal
chromaticKeyTraversal = [0..11]
fourthsKeyTraversal = [ n `mod` 12 | n <- [0,5..] ]
fifthsKeyTraversal = [ n `mod` 12 | n <- [0,7..] ]

fourNoteArpeggioExercise :: Scale -> KeyTraversal -> Exercise
fourNoteArpeggioExercise scale ktrav = Exercise $
    [ fourNoteArpeggioSegment (baseScale (48 + k) scale) | k <- ktrav ]

invertedFourNoteArpeggioExercise :: Scale -> KeyTraversal -> Exercise
invertedFourNoteArpeggioExercise scale ktrav = Exercise $
    [ invertedFourNoteArpeggioSegment (baseScale (48 + k) scale) | k <- ktrav ]

checkSegment :: Double -> Segment -> StreamProc Note Note Segment
checkSegment tempo (Segment notes) =
    mergeInner (mapO (const metronomeTone) (metronome tempo)) $ Segment <$> go 0 
    where
    metronomeTone = NoteOn 84 100
    fromBeats beats = 60 * beats / tempo
    toBeats t = t * tempo / 60
    beats = fromIntegral . ceiling . maximum $ map fst notes
    phraseTime = fromBeats beats
    go !itime
        | itime >= phraseTime = return []
        | otherwise = do
            m <- waitWithTime (TimeDiff (phraseTime - itime))
            case m of
                Nothing -> return []
                Just (TimeDiff dt, n) -> do
                    output n
                    case n of
                        NoteOn n' _ -> ((toBeats (itime+dt), n'):) <$> go (itime+dt)
                        _ -> go (itime+dt)
        
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


