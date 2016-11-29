{-# LANGUAGE OverloadedStrings, OverloadedLists, MultiWayIf #-}

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Data.String (fromString)
import GHC.Exts (fromList)

import qualified Control.Monad.Random as Rand
import qualified Data.Aeson as J
import qualified Data.Map as Map
import qualified System.MIDI as SysMid
import qualified Web.Scotty as S

import qualified MIDI
import qualified Syntax
import qualified Semantics
import qualified Levels

data Session = Session {
    sessionLevel :: Levels.Level,
    sessionAttempt :: Maybe (Syntax.Exp, String -> Bool) }

main :: IO ()
main = do
  sessionCounter <- liftIO $ newMVar 0
  sessions <- liftIO $ newMVar (Map.empty :: Map.Map Int Session)
  S.scotty 3000 $ do
    S.get "/" $ do
        S.setHeader "Content-type" "text/html"
        S.file "index.html"

    S.get "/resources/:file" $ do
        S.file . ("resources/"<>) =<< S.param "file"

    S.get "/play" $ do
        devname <- S.param "dev"
        expstr <- S.param "exp"

        case Syntax.parseString Syntax.parse expstr of
            Left err -> S.json $ J.object [ 
                "type" J..= J.String "error",
                "message" J..= J.String (fromString (show err)) ]
            Right expr -> do
                dev <- liftIO $ MIDI.connectOutput devname
                liftIO . forkIO $ MIDI.playNotes 1 (Semantics.evalExp expr) dev
                S.json $ J.object [
                    "type" J..= J.String "success" ]

    S.get "/devices" $ do
        devnames <- liftIO $ mapM SysMid.getName =<< SysMid.enumerateDestinations
        S.json $ J.Array (J.String . fromString <$> fromList devnames)

    S.get "/level" $ do
        levelname <- S.param "name"
        level <- return . head . filter ((levelname ==) . Levels.levelName) $ Levels.levels
        sessionid <- liftIO $ modifyMVar sessionCounter (\i -> return (i+1,i))
        liftIO $ modifyMVar_ sessions (return . Map.insert sessionid (Session level Nothing))

        S.json $ J.object [
            "name" J..= J.String (fromString (Levels.levelName level)),
            "desc" J..= J.String (fromString (Levels.levelDesc level)),
            "session" J..= J.Number (fromIntegral sessionid) ]

    S.get "/trylevel" $ do
        devname <- S.param "dev"
        sessionid <- S.param "session"
        answer <- S.param "answer"

        session <- liftIO $ (Map.! sessionid) <$> readMVar sessions
        dev <- liftIO $ MIDI.connectOutput devname

        (S.json =<<) . liftIO $ case sessionAttempt session of
            Nothing -> do
                (expr, ans) <- Rand.evalRandIO (Levels.levelSpec (sessionLevel session))
                forkIO $ MIDI.playNotes 1 (Semantics.evalExp expr) dev
                modifyMVar_ sessions $
                    return . Map.insert sessionid (session { sessionAttempt = Just (expr, ans) })
                return $ J.object []
            Just (expr, ans) -> do
                forkIO $ MIDI.playNotes 1 (Semantics.evalExp expr) dev
                if | null answer -> return $ J.object []
                   | ans answer -> do
                        modifyMVar_ sessions $
                            return . Map.insert sessionid (session { sessionAttempt = Nothing })
                        return $ J.object [
                            "correct" J..= J.Bool True,
                            "notation" J..= Syntax.pretty expr ]
                   | otherwise -> return $ J.object [
                        "correct" J..= J.Bool False ]

    S.get "/levels" $ do
        S.json . J.Array $ J.String . fromString . Levels.levelName <$> fromList Levels.levels
