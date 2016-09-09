{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Exercises where

import Control.Monad (forever)
import Control.Arrow (second)
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

metronomeTone = NoteOn 84 100

recordSegment :: Double -> Segment -> StreamProc Note Note Segment
recordSegment tempo (Segment notes) =
    mergeInner (mapO (const metronomeTone) (metronome tempo)) $ Segment <$> go 0 
    where
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

metronomeIntro :: Double -> Int -> StreamProc Note Note a -> StreamProc Note Note a
metronomeIntro tempo beats cont = do
    mergeInner (mapO (const metronomeTone) (metronome tempo)) 
               (waitTime (TimeDiff (60 * fromIntegral beats / tempo)))
    cont

data SegDiff = SegDiff {
    missedNotes :: Int,
    extraNotes :: Int,
    totalNotes :: Int,
    sqrTimingDiff :: Double
}
    deriving (Show)

instance Monoid SegDiff where
    mempty = SegDiff 0 0 0 0
    SegDiff a b c d `mappend` SegDiff a' b' c' d' = SegDiff (a+a') (b+b') (c+c') (d+d')

findSplit :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
findSplit p [] = Nothing
findSplit p (x:xs) 
    | Just y <- p x = Just (y, xs)
    | otherwise     = fmap (second (x:)) (findSplit p xs)

diffSegments :: Segment -> Segment -> SegDiff
diffSegments (Segment ms) (Segment ns) = diffSegments' ms ns

diffSegments' :: [(Double,Int)] -> [(Double,Int)] -> SegDiff
diffSegments' [] ns = mempty { extraNotes = length ns }
diffSegments' ms [] = mempty { missedNotes = length ms }
diffSegments' ((mtime,mnote):ms) ns
    | Just (d,ns') <- findSplit metric ns = d `mappend` diffSegments' ms ns'
    | otherwise = mempty { missedNotes = 1 } `mappend` diffSegments' ms ns
    where
    metric (time,note)
        | note == mnote && diffsq < (1/2)^2 
            = Just $ mempty { totalNotes = 1, sqrTimingDiff = diffsq }
        | otherwise = Nothing
        where
        diffsq = (time-mtime)^2
        
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


