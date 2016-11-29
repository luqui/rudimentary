{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Data.Monoid ((<>))
import Control.Monad.Trans (liftIO)
import Data.String (fromString)
import GHC.Exts (fromList)

import qualified Data.Aeson as J
import qualified System.MIDI as SysMid
import qualified Web.Scotty as S

import qualified MIDI
import qualified Syntax
import qualified Semantics

main :: IO ()
main = S.scotty 3000 $ do
    S.get "/" $ do
        S.setHeader "Content-type" "text/html"
        S.file "index.html"

    S.get "/resources/:file" $ do
        S.file . ("resources/"<>) =<< S.param "file"

    S.get "/play" $ do
        devname <- S.param "dev"
        expstr <- S.param "exp"

        case Syntax.parseString Syntax.parse expstr of
            Left err -> S.html . fromString . show $ err
            Right expr -> do
                dev <- liftIO $ MIDI.connectOutput devname
                liftIO $ MIDI.playNotes 1 (Semantics.evalExp expr) dev
        S.json $ J.object []

    S.get "/devices" $ do
        devnames <- liftIO $ mapM SysMid.getName =<< SysMid.enumerateDestinations
        S.json $ J.Array (J.String . fromString <$> fromList devnames)

