--------------------------------------------------------------------------------

-- | Server communicating with the Decks frontend editor.
--
module Decks.Server where

import           Decks.Logging
import           Decks.Server.API               ( DecksAPI )
import           Decks.Server.Types

import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Network.Wai.Handler.Warp       ( run )
import           Servant
import           System.Directory               ( doesFileExist )

--------------------------------------------------------------------------------

serverPort :: Int
serverPort = 8081

runServer :: FilePath -> IO ()
runServer path = do
    logMsg LogInfo
        $  "Starting Decks server on port "
        <> (T.pack . show) serverPort
        <> "..."
    run 8081 (app path)

--------------------------------------------------------------------------------

app :: FilePath -> Application
app path = serve decksAPI (server path)

decksAPI :: Proxy DecksAPI
decksAPI = Proxy

server :: FilePath -> Server DecksAPI
server path = do
    fExists <- liftIO $ doesFileExist path
    content <- if fExists
        then liftIO $ Just <$> TIO.readFile path
        else pure Nothing
    pure $ Presentation content

--------------------------------------------------------------------------------
