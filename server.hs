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

import qualified Levels
import qualified MIDI
import qualified Params
import qualified Syntax
import qualified Semantics

data Session = Session {
    sessionParams :: Map.Map String String,
    sessionAttempt :: Maybe (Syntax.Exp, String -> Maybe Bool) }

selectJSON :: [(String, Params.SelectWidget, String)] -> J.Value
selectJSON outs = J.Array . fromList $ do
    (name, widget, value) <- outs
    return $ J.object [
        "label" J..= J.String (fromString name),
        "title" J..= J.String (fromString (Params.swTitle widget)),
        "value" J..= J.String (fromString value),
        "options" J..= J.Array (fromList [
            J.object [ "name" J..= J.String (fromString name),
                       "desc" J..= J.String (fromString desc) ]
            | (name, desc) <- Params.swOptions widget ])
      ]

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

    S.get "/devices" $ do
        devnames <- liftIO $ mapM SysMid.getName =<< SysMid.enumerateDestinations
        S.json $ J.Array (J.String . fromString <$> fromList devnames)

    S.get "/level" $ do
        Just params <- J.decode <$> S.param "params"
        sessionid <- liftIO $ modifyMVar sessionCounter (\i -> return (i+1,i))

        let (_, outs) = Params.runParams Levels.levels params
        let session = Session (Params.outputToInput outs) Nothing
        liftIO $ modifyMVar_ sessions (return . Map.insert sessionid session)

        S.json $ J.object [
            "session" J..= J.Number (fromIntegral sessionid),
            "content" J..= selectJSON outs ]
            
    S.get "/trylevel" $ do
        devname <- S.param "dev"
        sessionid <- S.param "session"
        answer <- S.param "answer"
        tempo <- S.param "tempo"

        let filterSession s | answer == "GIVEUP" = s { sessionAttempt = Nothing }
                            | otherwise = s

        session <- liftIO $ filterSession . (Map.! sessionid) <$> readMVar sessions
        dev <- liftIO $ MIDI.connectOutput devname

        (S.json =<<) . liftIO $ case sessionAttempt session of
            Nothing -> do
                let (dist, _) = Params.runParams Levels.levels (sessionParams session)
                (expr, ans) <- Rand.evalRandIO dist
                forkIO $ MIDI.playNotes (60/tempo) (Semantics.evalExp expr) dev
                modifyMVar_ sessions $
                    return . Map.insert sessionid (session { sessionAttempt = Just (expr, ans) })
                return $ J.object []
            Just (expr, ans) -> do
                forkIO $ MIDI.playNotes (60/tempo) (Semantics.evalExp expr) dev
                let grade = ans answer
                if | null answer -> return $ J.object []
                   | Just True <- grade -> do
                        modifyMVar_ sessions $
                            return . Map.insert sessionid (session { sessionAttempt = Nothing })
                        return $ J.object [
                            "correct" J..= J.Bool True,
                            "notation" J..= Syntax.pretty expr ]
                   | Just False <- grade -> return $ J.object [
                        "correct" J..= J.Bool False ]
                   | otherwise -> return $ J.object [
                        "correct" J..= J.Null ]
