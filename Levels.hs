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
    intervals,
    chordQualities ]

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
                     "is still 2.  It's the <i>first</i> note that defines the root.  When the ",
                     "second note is lower, it may be useful to think of the interval with the ",
                     "bottom note as the root, and then invert the interval (e.g. <code>M3</code> ",
                     " &rarr; <code>m6</code>."])  
            [ (show r, r) | r <- [1..5] ]
        let minNote = -7 * (octRange `div` 2)
        let maxNote = 7 * ((octRange+1) `div` 2)

        keyDist <- selectKey
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
        , ("Chromatic", chromaticHelpText, chrDist) ]
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
        Semantics.applyScale cMajor i `mod` 12 == Semantics.applyScale cMajor j `mod` 12
    cMajor = Semantics.renderScale (Scale (Note 0) (Mode Natural 7))

    chromaticHelpText = concat [
        "Aribtrary intervals will be generated. Your answer will include the interval and ",
        "the quality.  1,4,5 can be either <i>perfect</i> (notated <code>P</code>), ",
        "<i>augmented</i> (<code>aug</code>) to sharp by one, or <i>diminished</i> ",
        "(<code>dim</code>) to flat by one.  2,3,6,7 can be either <i>major</i> (<code>M</code>) ", 
        "as from the major scale, <i>minor</i> (<code>m</code>) to flat by 1.  So various valid ",
        "answers are <code>P4</code>, <code>m7</code> (so-called \"dominant 7\"), <code>M2</code>, ",
        "but not <code>m5</code> or <code>P3</code>." ]


selectKey :: Params (Dist Note)
selectKey = select "Key"
    [ ("C", "The reference tone will always be the same, C", pure (Note 0))
    , ("Random", "The reference tone will be a random note", Note <$> uniform [0..11]) ]


chordQualities :: Sublevel
chordQualities = Sublevel {
    slName = "Chord Qualities",
    slDesc = "I will play a chord, and you tell me what kind of chord it is.",
    slLevel = do
        selectFrom <- select "Chord Types" 
            [ ("Major, Minor", 
               "Just distinguish between major (<code>maj</code>) and minor (<code>min</code>).",
                ["maj", "min"])
            , ("Triads", 
               "Add augmented (<code>aug</code>) and diminished (<code>dim</code>) into the mix.",
                ["maj", "min", "aug", "dim"])
            , ("7 Chords",
               "Distinguish between major 7 (<code>maj7</code>), minor 7 (<code>min7</code>), and "++
               "dominant 7 (<code>dom7</code>) chords.",
               ["maj7", "min7", "dom7"])
            , ("Diminished chords",
               "Distinguish between regular diminished (<code>dim</code>), half-diminished "++
               "(<code>halfdim</code>) and fully diminished (<code>dim7</code>) chords.",
               ["dim", "halfdim", "dim7"])
            , ("Exotic chords",
               "Distinguish between minor-major (<code>minmaj</code>), augmented 7 "++
               "(<code>aug7</code>), and fully diminished chords (<code>dim7</code>).",
               ["aug7", "minmaj", "dim7"])
            , ("Tetrachords",
               "All the four-note chord forms covered so far: <code>maj7</code>, "++
               "<code>min7</code>, <code>dom7</code>, <code>dim</code>, <code>halfdim</code>, "++
               "<code>dim7</code>, <code>aug7</code>, and <code>minmaj</code>.",
               ["maj7", "min7", "dom7", "halfdim", "aug7", "minmaj", "dim7"])
            , ("All chords",
               "Tetrachords and triads at the same time.",
               (Map.keys chordTypes)) ]

        voicing <- selectVoicing
        keyDist <- selectKey

        referenceTone <- select' "Reference Tone"
            "Whether I play the root of the chord first."
            [ ("Yes", DEConcat (DERun [Degree (-7) 0])), ("No", id) ]
        
        return $ do
            key <- keyDist
            chord <- uniform selectFrom
            notes <- voicing (chordTypes Map.! chord)
            return (Exp (Scale key (Mode Natural 7)) 
                        (referenceTone (foldr1 DEParallel [ DERun [n] | n <- notes ])),
                    grade selectFrom chord)
    }
    where
    chordTypes = Map.fromList [
        ("maj", [Degree 0 0, Degree 2 0, Degree 4 0]),
        ("min", [Degree 0 0, Degree 2 (-1), Degree 4 0]),
        ("dim", [Degree 0 0, Degree 2 (-1), Degree 4 (-1)]),
        ("aug", [Degree 0 0, Degree 2 0, Degree 4 1]),
        ("dom7", [Degree 0 0, Degree 2 0, Degree 4 0, Degree 6 (-1)]),
        ("maj7", [Degree 0 0, Degree 2 0, Degree 4 0, Degree 6 0]),
        ("min7", [Degree 0 0, Degree 2 (-1), Degree 4 0, Degree 6 (-1)]),
        ("halfdim", [Degree 0 0, Degree 2 (-1), Degree 4 (-1), Degree 6 (-1)]),
        ("dim7", [Degree 0 0, Degree 2 (-1), Degree 4 (-1), Degree 6 (-2)]),
        ("aug7", [Degree 0 0, Degree 2 0, Degree 4 1, Degree 6 (-1)]),
        ("minmaj", [Degree 0 0, Degree 2 (-1), Degree 4 0, Degree 6 0]) ]

    grade selectFrom correct ans
        | ans `elem` selectFrom = Just (correct == ans)
        | otherwise = Nothing


selectVoicing :: Params ([Degree] -> Dist [Degree])
selectVoicing = select "Voicing"
    [ ("Root",
       "The root of the chord is always the lowest, followed by the 3rd, 5th, and 7th in " ++
       "ascending order.  This is the simplest voicing and should be used to get " ++
       "familiar with new chord qualities.",
       return)
    , ("Closed",
       "All the notes of the chord are as close together as possible, but the root might " ++
       "not be on the bottom.",
       randomInversion)
    , ("Open",
       "The notes of the chord are spread out with wider intervals.",
       openVoicing) ]
    where
    randomInversion chord = do
        invUp <- uniform [0..length chord-1]
        return $ [ Degree (n+7) alt | Degree n alt <- take invUp chord ] ++ drop invUp chord

    openVoicing chord = do
        mapM (\(Degree n alt) -> Degree <$> ((n+) <$> uniform [-7,0,7]) <*> pure alt) chord

