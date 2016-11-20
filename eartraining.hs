{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, filterM)
import Control.Monad.Trans (liftIO)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import System.Random (randomRIO)
import qualified Data.Map as Map
import qualified System.Console.Haskeline as RL
import qualified System.MIDI as MIDI

data ScaleGenus
    = Major
    | Melodic
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

genusIntervals :: ScaleGenus -> [Int]
genusIntervals Major = [2,1,2,2,2,1,2]   -- dorian, the center
genusIntervals Melodic = [2,2,1,2,1,2,2] -- major-minor

genusSize :: ScaleGenus -> Int
genusSize = length . genusIntervals

data Mode = Mode ScaleGenus Int  -- 1-based mode
    deriving (Eq, Ord, Read, Show)

modeIntervals :: Mode -> [Int]
modeIntervals (Mode g m) = trunc intervals (drop (m-1) (cycle (genusIntervals g)))
    where
    intervals = genusIntervals g
    trunc = zipWith (const id)

newtype Note = Note Int
    deriving (Eq, Ord)

baseNoteNames :: [(String, Int)]
baseNoteNames = [("C", 0), ("D", 2), ("E", 4), ("F", 5), ("G", 7), ("A", 9), ("B", 11)]

noteName :: Note -> String
noteName (Note i) = noteMap Map.! i
    where
    noteMap = Map.fromList . map swap $ [ (name ++ "#", note+1) | (name,note) <- baseNoteNames ] ++ baseNoteNames

instance Show Note where show = noteName

parseNote :: String -> [(Note, String)]
parseNote (n:'b':s)
    | Just t <- Map.lookup [n] (Map.fromList baseNoteNames) = [(Note ((t-1) `mod` 12), s)]
parseNote (n:'#':s)
    | Just t <- Map.lookup [n] (Map.fromList baseNoteNames) = [(Note ((t+1) `mod` 12), s)]
parseNote (n:s)
    | Just t <- Map.lookup [n] (Map.fromList baseNoteNames) = [(Note t,s)]
parseNote _ = []

instance Read Note where readsPrec _ = parseNote

data Scale = Scale Note Mode
    deriving (Eq, Ord, Read, Show)

renderScale :: Scale -> [Int]
renderScale (Scale (Note note0) scaleType) = scanl (+) (60+note0) (modeIntervals scaleType)

data Game = Game {
    playC4 :: IO (),
    scaleGame :: IO ()
}

startGame :: IO Game
startGame = do
    dest <- connectOutput "IAC Bus 1"
    return $ Game {
        playC4 = playNotes 1 [60] dest,
        scaleGame = scaleGameReal dest
    }

scaleGameReal :: MIDI.Connection -> IO ()
scaleGameReal conn = do
    genus <- choice [minBound..maxBound]
    mode <- randomRIO (1, genusSize genus)
    let startNote = Note 0
    let scale = Scale startNote (Mode genus mode)
    RL.runInputT RL.defaultSettings (iter scale conn)
    where
    iter scale conn = do
        liftIO $ playNotes 0.25 (renderScale scale) conn
        liftIO $ putStrLn "Enter an expression to guess or 'r' to repeat"
        Just ans <- RL.getInputLine "> "
        if ans == "r"
            then iter scale conn
            else do
                case maybeRead ans of
                    Nothing -> liftIO (putStrLn "Parse error") >> iter scale conn
                    Just e -> if e == scale 
                                 then liftIO $ putStrLn "Correct!" >> playNotes 0.25 (renderScale scale) conn
                                 else liftIO (putStrLn "Incorrect!") >> iter scale conn
    

connectOutput :: String -> IO MIDI.Connection
connectOutput destName = do
    destinations <- MIDI.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> MIDI.getName d) destinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< MIDI.getName destination
    return conn

playNotes :: Double -> [Int] -> MIDI.Connection -> IO ()
playNotes dt notes conn = do
    forM_ notes $ \note -> do
        MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn note 64))
        threadDelay (floor (10^6 * dt))
        MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOff note 0))

choice :: [a] -> IO a
choice xs = do
    i <- randomRIO (0, length xs-1)
    return $ xs !! i

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = listToMaybe [ x | (x,"") <- reads s ]
