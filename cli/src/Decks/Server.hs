--------------------------------------------------------------------------------

-- | Backend server which communicates with the Decks frontend editor, via a
-- WebSocket connection.
--
module Decks.Server where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Control.Monad.Extra            ( whenM )
import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time
import           Decks.Logging
import           Decks.Server.API
import           Decks.Server.Types
import           Decks.Utils                    ( URL )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors
import           Network.WebSockets      hiding ( serverPort )
import           Servant
import           System.Directory               ( doesFileExist
                                                , getModificationTime
                                                )

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
--
-- When no message is sent or received within this timeframe, the connection is
-- timed out and closes.
wsPingInterval :: Int
wsPingInterval = 5 * 60

-- | Number of seconds to delay the server thread for, between sending messages.
wsUpdateInterval :: Int
wsUpdateInterval = 1

--------------------------------------------------------------------------------

-- | The WAI application serving the Decks backend.
app :: FilePath -> Application
app path = simpleCors $ serve decksAPI (server path)

server :: FilePath -> Server DecksAPI
server path = streamData
  where
    streamData :: MonadIO m => Connection -> m ()
    streamData conn =
        liftIO $ withPingThread conn wsPingInterval (pure ()) $ forever $ do
            whenM shouldUpdateFrontend $ do
                content <- liftIO $ Just <$> TIO.readFile path
                sendTextData conn (Presentation content)

            threadDelay (wsUpdateInterval * 1000000)

    shouldUpdateFrontend :: IO Bool
    shouldUpdateFrontend = do
        fileExists <- doesFileExist path
        if fileExists
            then do
                modTime <- getModificationTime path
                curTime <- getCurrentTime

                -- Decks output was last checked now minus the update interval
                let lastCheckedTime = addUTCTime
                        ( secondsToNominalDiffTime
                        . negate
                        . fromIntegral
                        $ wsUpdateInterval
                        )
                        curTime
                pure $ modTime > lastCheckedTime
            else pure False


--------------------------------------------------------------------------------
