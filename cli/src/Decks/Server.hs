--------------------------------------------------------------------------------

-- | Backend server which communicates with the Decks frontend editor, via a
-- WebSocket connection.
--
module Decks.Server where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Decks.Logging
import           Decks.Server.API
import           Decks.Server.Types
import           Decks.Utils                    ( URL )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors
import           Network.WebSockets      hiding ( serverPort )
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
    run serverPort (app path)

--------------------------------------------------------------------------------

-- | Number of seconds to wait between pinging via WebSocket.
wsInterval :: Int
wsInterval = 10

-- | Number of /microseconds/ to delay the server thread for, between sending
-- messages.
serverInterval :: Int
serverInterval = 1 * 1000000

--------------------------------------------------------------------------------

-- | The WAI application serving the Decks backend.
app :: FilePath -> Application
app path = simpleCors $ serve decksAPI (server path)

server :: FilePath -> Server DecksAPI
server path = streamData
  where
    streamData :: (MonadIO m) => Connection -> m ()
    streamData conn = do
        fExists <- liftIO $ doesFileExist path
        content <- if fExists
            then liftIO $ Just <$> TIO.readFile path
            else pure Nothing
        let pres = Presentation content

        liftIO $ withPingThread conn wsInterval (pure ()) $ forever $ do
            sendTextData conn pres
            threadDelay serverInterval

--------------------------------------------------------------------------------
