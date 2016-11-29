{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, BangPatterns #-}

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
import qualified Control.Monad.Random as Rand

import Syntax
import Semantics

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


data Game = Game {
    game :: Dist Exp -> Double -> IO (),
    audition :: String -> Double -> IO ()
}


startGame :: IO Game
startGame = do
    dest <- connectOutput "IAC Bus 1"
    return $ Game {
        game = gameFromSchema dest,
        audition = \inp tempo -> case parseString parse inp of
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
                case parseString parse (drop (length "audition ") ans) of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter exp notes
                    Right exp' -> liftIO (playNotes (15/tempo) (evalExp exp') conn) >> iter exp notes
              | otherwise ->
                case parseString parse ans of
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
    
