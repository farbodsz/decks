--------------------------------------------------------------------------------

-- | Server communicating with the Decks frontend editor.
--
module Decks.Server where

import           Decks.Logging
import           Decks.Server.API               ( DecksAPI )
import           Decks.Server.Types
import           Decks.Utils                    ( URL )

import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Network.Wai.Handler.Warp       ( run )
import           Servant

import           System.Directory               ( doesFileExist )

--------------------------------------------------------------------------------

serverPort :: Int
serverPort = 8081

runServer :: FilePath -> URL -> IO ()
runServer path frontUrl = do
    logMsg LogInfo
        $  "Starting Decks server on port "
        <> (T.pack . show) serverPort
        <> "..."
    logMsg LogInfo $ "Using frontend at " <> (T.pack . show) frontUrl
    run 8081 (app path frontUrl)

--------------------------------------------------------------------------------

app :: FilePath -> URL -> Application
app path url = serve decksAPI (server path url)

decksAPI :: Proxy DecksAPI
decksAPI = Proxy

server :: FilePath -> URL -> Server DecksAPI
server path frontUrl = do
    fExists <- liftIO $ doesFileExist path
    content <- if fExists
        then liftIO $ Just <$> TIO.readFile path
        else pure Nothing
    pure $ addHeader frontUrl $ Presentation content

--------------------------------------------------------------------------------
