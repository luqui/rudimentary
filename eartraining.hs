{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, BangPatterns, DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.MVar
import Control.Monad (forM_, filterM, when, (<=<), join)
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf, intercalate, sort)
import Data.Maybe (listToMaybe)
import Data.Tuple (swap)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified System.Console.Haskeline as RL
import qualified System.MIDI as MIDI
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Control.Monad.Random as Rand

type Dist = Rand.Rand Rand.StdGen

dist :: [(Double, a)] -> Dist a
dist choices = do
    x <- Rand.getRandomR (0, sum (map fst choices))
    let choose accum [] = error "no choices"
        choose accum ((p,a):as)
            | x <= accum + p  = return a
            | otherwise       = choose (accum+p) as
    choose 0 choices

uniform :: [a] -> Dist a
uniform = dist . map (1,)

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

class Pretty a where
    pretty :: a -> String

data ScaleGenus
    = Natural
    | Melodic
    | Diminished
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

instance Pretty ScaleGenus where
    pretty Natural = "nat"
    pretty Melodic = "mel"
    pretty Diminished = "dim"

genusParser :: Parser ScaleGenus
genusParser = P.choice [
    Natural <$ P.symbol tok "nat",
    Melodic <$ P.symbol tok "mel",
    Diminished <$ P.symbol tok "dim" ]

genusIntervals :: ScaleGenus -> [Int]
genusIntervals Natural = [2,1,2,2,2,1,2]      -- dorian, the center
genusIntervals Melodic = [2,2,1,2,1,2,2]      -- major-minor
genusIntervals Diminished = [2,1,2,1,2,1,2,1] -- 2-1 (diminished does not have a center mode!)

genusSize :: ScaleGenus -> Int
genusSize = length . genusIntervals

data Mode = Mode ScaleGenus Int  -- 1-based mode
    deriving (Eq, Ord, Show)

instance Pretty Mode where
    pretty (Mode g n) = pretty g ++ " " ++ show n

modeIntervals :: Mode -> [Int]
modeIntervals (Mode g m) = trunc intervals (drop (m-1) (cycle (genusIntervals g)))
    where
    intervals = genusIntervals g
    trunc = zipWith (const id)

newtype Note = Note Int
    deriving (Eq, Ord, Show)

instance Pretty Note where
    pretty = noteName

baseNoteNames :: [(String, Int)]
baseNoteNames = [("C", 0), ("D", 2), ("E", 4), ("F", 5), ("G", 7), ("A", 9), ("B", 11)]

noteName :: Note -> String
noteName (Note i) = noteMap Map.! i
    where
    noteMap = Map.fromList . map swap $ [ (name ++ "#", note+1) | (name,note) <- baseNoteNames ] ++ baseNoteNames

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

instance Pretty Scale where
    pretty (Scale n m) = pretty n ++ " " ++ pretty m

scaleParser :: Parser Scale
scaleParser = do
    baseNote <- noteParser
    genus <- genusParser
    mode <- fromIntegral <$> P.natural tok
    when (mode < 1 || mode > genusSize genus) $ fail "Invalid mode"
    return $ Scale baseNote (Mode genus mode)

renderScale :: Scale -> [Int]
renderScale (Scale (Note note0) scaleType) = init $ scanl (+) (60+note0) (modeIntervals scaleType)


data Degree 
    = Degree Int Int   -- degree(0-based) accidental
    | Rest
    deriving (Eq, Ord, Show)

instance Pretty Degree where
    pretty (Degree n acc) = showacc ++ show (1+n)
        where
        showacc | acc < 0 = replicate (-acc) 'b'
                | acc >= 0 = replicate acc '#'

shift :: Degree -> Degree -> Degree
shift Rest _ = Rest
shift _ Rest = Rest
shift (Degree a acca) (Degree b accb) = Degree (a+b) (acca+accb)

(<>) :: Media Degree -> Media Degree -> Media Degree
(<>) = liftA2 shift

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
    | DEParallel DegreeExp DegreeExp
    | DEConcat DegreeExp DegreeExp
    | DEOp DegreeOperator
    deriving (Show)

instance Pretty DegreeExp where
    pretty (DERun ds) = intercalate "," (map pretty ds)
    pretty (DEParallel a b) = pretty a ++ " & " ++ pretty b
    pretty (DEMult a b) = pretty a ++ " " ++ pretty b
    pretty (DEConcat a b) = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
    pretty (DEOp op) = pretty op

data DegreeOperator
    = DOIdentity
    | DOInvert
    | DORetrograde
    deriving (Show)

instance Pretty DegreeOperator where
    pretty DOIdentity = "Id"
    pretty DOInvert = "Inv"
    pretty DORetrograde = "Ret"

degreeExpParser :: Parser DegreeExp
degreeExpParser = catExp
    where
    atomic = P.choice [
        P.parens tok degreeExpParser,
        DERun <$> degreeRunParser,
        DEOp <$> operator ]
    parExp = binOp (DEParallel <$ P.symbol tok "&") atomic
    opExp = binOp (pure DEMult) parExp
    catExp = binOp (DEConcat <$ P.symbol tok "+") opExp
    binOp op p = flip ($) <$> p <*> P.option id (flip <$> op <*> binOp op p)
    operator = P.choice [
        P.try (DOIdentity <$ P.symbol tok "Id"),
        P.try (DOInvert <$ P.symbol tok "Inv"),
        P.try (DORetrograde <$ P.symbol tok "Ret") ]

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
    

data Exp = Exp Scale DegreeExp
    deriving (Show)

instance Pretty Exp where
    pretty (Exp scale de) = pretty scale ++ " " ++ pretty de

expParser :: Parser Exp
expParser = Exp <$> scaleParser <*> degreeExpParser

evalExp :: Exp -> Media Int
evalExp (Exp scale degexp) = fmap (applyScale (renderScale scale)) (interpDegreeExp degexp)


data Game = Game {
    game :: Dist Exp -> Double -> IO (),
    audition :: String -> Double -> IO ()
}


startGame :: IO Game
startGame = do
    dest <- connectOutput "IAC Bus 1"
    return $ Game {
        game = gameFromSchema dest,
        audition = \inp tempo -> case P.parse (expParser <* P.eof) "<input>" inp of
            Right exp -> playNotes (15/tempo) (evalExp exp) dest
            Left err -> print err
    }

getLevel :: String -> Dist Exp
getLevel s
    | Just level <- lookup s levels = level
    | otherwise = error "No such level"

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
        go' 0 accum = return []
        go' n accum = do
            ni <- uniform [1..3]
            ((accum+ni) :) <$> go' (n-1) (accum+ni)

    adjust m n
        | n >= m = n+1
        | otherwise = n
        

checkLength :: Int -> Int -> [a] -> Bool
checkLength !lo !hi [] = lo <= 0
checkLength !lo !hi (x:xs) 
    | hi > 0 = checkLength (lo-1) (hi-1) xs
    | otherwise = False


gameFromSchema :: MIDI.Connection -> Dist Exp -> Double -> IO ()
gameFromSchema conn expdist tempo = do
    exp <- Rand.evalRandIO expdist
    let notes = evalExp exp
    play notes
    RL.runInputT RL.defaultSettings (iter exp notes)
    where
    play notes = playNotes delay notes conn
    iter exp notes = do
        liftIO $ putStrLn "Enter an expression to guess, or 'r', 'slow', 'ref'"
        Just ans <- RL.getInputLine "> "
        case ans of
            "r" -> liftIO (play notes) >> iter exp notes
            "slow" -> liftIO (playNotes (2*delay) notes conn) >> iter exp notes
            "ref" -> liftIO (putStrLn "C" >> playNotes 1 (Prim 60) conn) >> iter exp notes
            "giveup" -> liftIO (putStrLn (pretty exp))
            _ | "audition " `isPrefixOf` ans -> 
                case P.parse (expParser <* P.eof) "<audition>" (drop (length "audition ") ans) of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter exp notes
                    Right exp' -> liftIO (playNotes (15/tempo) (evalExp exp') conn) >> iter exp notes
              | otherwise ->
                case P.parse (expParser <* P.eof) "<input>" ans of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter exp notes
                    Right exp' -> if evalExp exp' == notes
                                 then liftIO $ putStrLn ("Correct: " ++ pretty exp)  >> play notes
                                 else liftIO (putStrLn "Incorrect!" >> play (evalExp exp')) >> iter exp notes
    delay = 15/tempo -- the length of a sixteenth note
    

connectOutput :: String -> IO MIDI.Connection
connectOutput destName = do
    destinations <- MIDI.enumerateDestinations
    [destination] <- filterM (\d -> (destName ==) <$> MIDI.getName d) destinations
    conn <- MIDI.openDestination destination
    putStrLn . ("Connected to destintion " ++) =<< MIDI.getName destination
    return conn

midiLock :: IO a -> IO a
midiLock = \action -> do takeMVar lock ; x <- action ; putMVar lock () ; return x
    where
    {-# NOINLINE lock #-}
    lock = unsafePerformIO $ newMVar ()

playNotes :: Double -> Media Int -> MIDI.Connection -> IO ()
playNotes dt (Prim note) conn = do
    when (note /= 0) . midiLock $ MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOn note 64))
    threadDelay (floor (10^6 * dt))
    when (note /= 0) . midiLock $ MIDI.send conn (MIDI.MidiMessage 1 (MIDI.NoteOff note 0))
playNotes dt (m :+: m') conn = do
    playNotes dt m conn
    playNotes dt m' conn
playNotes dt (m :=: m') conn = do
    v <- newEmptyMVar
    v' <- newEmptyMVar
    t <- forkFinally (playNotes dt m conn) $ \_ -> putMVar v ()
    t' <- forkFinally (playNotes dt m' conn) $ \_ -> putMVar v' ()
    () <- takeMVar v
    () <- takeMVar v'
    return ()
    

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
