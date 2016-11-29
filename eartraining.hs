module Main where

import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)
import qualified System.Console.Haskeline as RL
import qualified System.MIDI as SysMid
import qualified Control.Monad.Random as Rand

import Syntax
import Semantics
import Levels
import MIDI


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
            Right e -> playNotes (15/tempo) (evalExp e) dest
            Left err -> print err
    }

gameFromSchema :: SysMid.Connection -> Dist Exp -> Double -> IO ()
gameFromSchema conn expdist tempo = do
    e <- Rand.evalRandIO expdist
    let notes = evalExp e
    play notes
    RL.runInputT RL.defaultSettings (iter e notes)
    where
    play notes = playNotes delay notes conn
    iter expr notes = do
        liftIO $ putStrLn "Enter an expression to guess, or 'r', 'slow', 'ref'"
        Just ans <- RL.getInputLine "> "
        case ans of
            "r" -> liftIO (play notes) >> iter expr notes
            "slow" -> liftIO (playNotes (2*delay) notes conn) >> iter expr notes
            "ref" -> liftIO (putStrLn "C" >> playNotes 1 (Prim 60) conn) >> iter expr notes
            "giveup" -> liftIO (putStrLn (pretty expr))
            _ | "audition " `isPrefixOf` ans -> 
                case parseString parse (drop (length "audition ") ans) of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter expr notes
                    Right exp' -> liftIO (playNotes (15/tempo) (evalExp exp') conn) >> iter expr notes
              | otherwise ->
                case parseString parse ans of
                    Left err -> liftIO (putStrLn $ "Parse error: " ++ show err) >> iter expr notes
                    Right expr' -> if evalExp expr' == notes
                                 then liftIO $ putStrLn ("Correct: " ++ pretty expr)  >> play notes
                                 else liftIO (putStrLn "Incorrect!" >> play (evalExp expr')) >> iter expr notes
    delay = 15/tempo -- the length of a sixteenth note
    

