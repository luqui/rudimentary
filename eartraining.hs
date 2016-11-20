{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, filterM, when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import System.Random (randomRIO)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified System.Console.Haskeline as RL
import qualified System.MIDI as MIDI
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

type Parser = P.Parsec String ()

data ScaleGenus
    = Natural
    | Melodic
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

tok = P.makeTokenParser $ P.LanguageDef {
        P.commentStart = "",
        P.commentEnd = "",
        P.commentLine = "",
        P.nestedComments = False,
        P.identStart = fail "no identifiers",
        P.identLetter = fail "no identifiers",
        P.opStart = fail "no operators",
        P.opLetter = fail "no operators",
        P.reservedNames = [],
        P.reservedOpNames = [],
        P.caseSensitive = True 
    }

genusParser :: Parser ScaleGenus
genusParser = P.choice [Natural <$ P.symbol tok "nat", Melodic <$ P.symbol tok "mel"]

genusIntervals :: ScaleGenus -> [Int]
genusIntervals Natural = [2,1,2,2,2,1,2]   -- dorian, the center
genusIntervals Melodic = [2,2,1,2,1,2,2] -- major-minor

genusSize :: ScaleGenus -> Int
genusSize = length . genusIntervals

data Mode = Mode ScaleGenus Int  -- 1-based mode
    deriving (Eq, Ord, Show)

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

noteParser :: Parser Note
noteParser = do
    i <- P.choice [ i <$ (P.symbol tok (map Char.toUpper n) <|> P.symbol tok (map Char.toLower n))
                  | (n,i) <- baseNoteNames ]
    acc <- P.choice [
        length <$> P.many1 (P.symbol tok "#"),
        negate . length <$> P.many1 (P.symbol tok "b"),
        return 0 ]
    return . Note $ (i + acc) `mod` 12

data Scale = Scale Note Mode
    deriving (Eq, Ord, Show)

scaleParser :: Parser Scale
scaleParser = do
    baseNote <- noteParser
    genus <- genusParser
    mode <- fromIntegral <$> P.integer tok
    when (mode < 1 || mode > genusSize genus) $ fail "Invalid mode"
    return $ Scale baseNote (Mode genus mode)

parseScale :: String -> Either P.ParseError Scale
parseScale = P.parse scaleParser "<input>"


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
    startNote <- Note <$> randomRIO (0,11)
    octave <- randomRIO (-1,1)
    let scale = Scale startNote (Mode genus mode)
    RL.runInputT RL.defaultSettings (iter octave scale conn)
    where
    iter octave scale conn = do
        liftIO $ playNotes 0.25 (map (+ (12*octave)) (renderScale scale)) conn
        liftIO $ putStrLn "Enter an expression to guess, 'r' to repeat, 'ref' for reference tone"
        Just ans <- RL.getInputLine "> "
        case ans of
            "r" -> iter octave scale conn
            "ref" -> liftIO (putStrLn "C" >> playNotes 1 [60] conn) >> iter octave scale conn
            _ -> do
                case parseScale ans of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter octave scale conn
                    Right exp -> if exp == scale 
                                 then liftIO $ putStrLn "Correct!" >> playNotes 0.25 (renderScale scale) conn
                                 else liftIO (putStrLn "Incorrect!") >> iter octave scale conn
    

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
