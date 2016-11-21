{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, filterM, when, (<=<))
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

data ScaleGenus
    = Natural
    | Melodic
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

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

accidentalParser :: Parser Int
accidentalParser = P.choice [
    length <$> P.many1 (P.symbol tok "#"),
    negate . length <$> P.many1 (P.symbol tok "b"),
    return 0 ]

noteParser :: Parser Note
noteParser = do
    i <- P.choice [ i <$ (P.symbol tok (map Char.toUpper n) <|> P.symbol tok (map Char.toLower n))
                  | (n,i) <- baseNoteNames ]
    acc <- accidentalParser
    return . Note $ (i + acc) `mod` 12

data Scale = Scale Note Mode
    deriving (Eq, Ord, Show)

scaleParser :: Parser Scale
scaleParser = do
    baseNote <- noteParser
    genus <- genusParser
    mode <- fromIntegral <$> P.natural tok
    when (mode < 1 || mode > genusSize genus) $ fail "Invalid mode"
    return $ Scale baseNote (Mode genus mode)

parseScale :: String -> Either P.ParseError Scale
parseScale = P.parse scaleParser "<input>"

renderScale :: Scale -> [Int]
renderScale (Scale (Note note0) scaleType) = init $ scanl (+) (60+note0) (modeIntervals scaleType)


data Degree 
    = Degree Int Int   -- degree(0-based) accidental
    | Rest
    deriving (Eq, Ord, Show)

shift :: Degree -> Degree -> Degree
shift Rest _ = Rest
shift _ Rest = Rest
shift (Degree a acca) (Degree b accb) = Degree (a+b) (acca+accb)

degreeParser :: Parser Degree
degreeParser = P.choice [
    flip Degree <$> accidentalParser <*> (subtract 1 . fromIntegral <$> P.natural tok),
    Rest <$ P.symbol tok "~" ]
    where
    deg = do
        i <- P.integer tok 
        when (i < 0) $ fail "degrees cannot be zero"
        return $ signum i * (abs i - 1)

degreeRunParser :: Parser [Degree]
degreeRunParser = P.choice [
    degreeParser `P.sepBy1` P.symbol tok ",",
    map (`Degree` 0) [0,1..7] <$ P.symbol tok "asc",
    map (`Degree` 0) [7,6..0] <$ P.symbol tok "desc" ]


(<>) :: [Degree] -> [Degree] -> [Degree]
lh <> rh = concatMap (\l -> map (shift l) rh) lh

invert :: Degree -> Degree
invert (Degree a acc) = Degree (negate a) (negate acc)
invert Rest = Rest

applyScale :: [Int] -> Degree -> Int
applyScale scale (Degree deg acc) = (scale !! (deg `mod` len)) + 12 * (deg `div` len) + acc
    where
    len = length scale
applyScale _ Rest = 0
    

data DegreeExp 
    = DERun [Degree]
    | DEMult DegreeExp DegreeExp
    | DEConcat DegreeExp DegreeExp
    | DEOp DegreeOperator
    deriving (Show)

data DegreeOperator
    = DOIdentity
    | DOInvert
    | DORetrograde
    deriving (Show)

degreeExpParser :: Parser DegreeExp
degreeExpParser = catExp
    where
    atomic = P.choice [
        P.parens tok degreeExpParser,
        DERun <$> degreeRunParser,
        DEOp <$> operator ]
    opExp = binOp (pure DEMult) atomic
    mulExp = binOp (DEMult <$ P.symbol tok "<>") opExp
    catExp = binOp (DEConcat <$ P.symbol tok "+") mulExp
    binOp op p = flip ($) <$> p <*> P.option id (flip <$> op <*> binOp op p)
    operator = P.choice [
        P.try (DOIdentity <$ P.symbol tok "Id"),
        P.try (DOInvert <$ P.symbol tok "Inv"),
        P.try (DORetrograde <$ P.symbol tok "Ret") ]

interpDegreeExp :: DegreeExp -> [Degree]
interpDegreeExp = ($ [Degree 0 0]) . go
    where
    go (DERun run) = (run <>)
    go (DEMult f g) = go f . go g
    go (DEConcat f g) = liftA2 (++) (go f) (go g)
    go (DEOp DOIdentity) = id
    go (DEOp DOInvert) = map invert
    go (DEOp DORetrograde) = reverse
    

expParser :: Parser [Int]
expParser = (map . applyScale . renderScale <$> scaleParser) 
        <*> (interpDegreeExp <$> degreeExpParser)
    where
    unMaybe p = p >>= maybe (fail "Nothing") return


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
    genus <- choice [Natural, Melodic]
    mode <- randomRIO (1, genusSize genus)
    startNote <- return $ Note 0 -- Note <$> randomRIO (0,11)
    octave <- randomRIO (-1,1)
    let scale = Scale startNote (Mode genus mode)
    let rendered@(firstNote:_) = renderScale scale ++ [firstNote+12]
    notes <- (12*octave + firstNote:) <$> randPerm (map (+ (12*octave)) (renderScale scale))
    RL.runInputT RL.defaultSettings (iter scale notes conn)
    where
    iter scale notes conn = do
        liftIO $ playNotes 0.5 notes conn
        liftIO $ putStrLn "Enter an expression to guess, 'r' to repeat, 'ref' for reference tone"
        Just ans <- RL.getInputLine "> "
        case ans of
            "r" -> iter scale notes conn
            "ref" -> liftIO (putStrLn "C" >> playNotes 1 [60] conn) >> iter scale notes conn
            _ -> do
                case parseScale ans of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter scale notes conn
                    Right exp -> if exp == scale 
                                 then liftIO $ putStrLn "Correct!" >> playNotes 0.25 notes conn
                                 else liftIO (putStrLn "Incorrect!") >> iter scale notes conn
    

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
        when (note /= 0) $ MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn note 64))
        threadDelay (floor (10^6 * dt))
        when (note /= 0) $ MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOff note 0))

choice :: [a] -> IO a
choice [] = error "choice []"
choice xs = do
    i <- randomRIO (0, length xs-1)
    return $ xs !! i

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = listToMaybe [ x | (x,"") <- reads s ]

randPerm :: [a] -> IO [a]
randPerm [] = return []
randPerm xs = do
    (s,ss) <- choice (selects xs)
    (s:) <$> randPerm ss

selects :: [a] -> [(a, [a])]
selects [] = []
selects (x:xs) = (x,xs) : (map.second) (x:) (selects xs)
