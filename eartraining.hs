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
import Levels


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
    
