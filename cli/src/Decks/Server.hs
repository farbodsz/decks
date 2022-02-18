--------------------------------------------------------------------------------

-- | Backend server which communicates with the Decks frontend editor, via a
-- WebSocket connection.
--
module Decks.Server where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( race_ )
import           Control.Monad                  ( forever )
import           Control.Monad.Extra            ( whenM )
import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time
import           Decks.Document                 ( DecksDocument(DecksDocument)
                                                , HtmlOutput(HtmlOutput)
                                                , docEditTextRange
                                                )
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

runServer :: DecksDocument -> HtmlOutput -> URL -> IO ()
runServer dslPath outPath frontUrl = do
    logMsg LogInfo
        $  "Starting Decks server on port "
        <> (T.pack . show) serverPort
        <> "..."
    logMsg LogInfo $ "Using frontend at " <> (T.pack . show) frontUrl
    run serverPort (app dslPath outPath)

--------------------------------------------------------------------------------

-- | Number of seconds to wait between pinging via WebSocket.
wsPingInterval :: Int
wsPingInterval = 30

-- | Number of seconds to delay the server thread for, between sending messages.
wsUpdateInterval :: Int
wsUpdateInterval = 1

--------------------------------------------------------------------------------

-- | The WAI application serving the Decks backend.
app :: DecksDocument -> HtmlOutput -> Application
app dslPath outPath = simpleCors $ serve decksAPI (server dslPath outPath)

server :: DecksDocument -> HtmlOutput -> Server DecksAPI
server (DecksDocument dslPath) (HtmlOutput outPath) = runWebSocket
  where
    runWebSocket :: MonadIO m => Connection -> m ()
    runWebSocket conn =
        liftIO $ withPingThread conn wsPingInterval (pure ()) $ race_
            (handlePushes conn)
            (handlePulls conn)

    -- | Sends updates to the frontend if the presentation DSL has changed.
    handlePushes :: Connection -> IO ()
    handlePushes conn = forever $ do
        whenM shouldUpdateFrontend $ do
            content <- Just <$> TIO.readFile outPath
            sendTextData conn (Presentation content)
        threadDelay (wsUpdateInterval * 1000000)

    shouldUpdateFrontend :: IO Bool
    shouldUpdateFrontend = do
        fileExists <- doesFileExist outPath
        if fileExists
            then do
                modTime <- getModificationTime outPath
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

    -- | Receives and processes notifications from the frontend.
    handlePulls :: Connection -> IO ()
    handlePulls conn = forever $ do
        (ReceivedNotification e_notif) <- receiveData conn
        case e_notif of
            Left err ->
                logMsg LogError
                    $  "Unable to process received notification: "
                    <> T.pack err
            Right res -> handleNotif res
        threadDelay (wsUpdateInterval * 1000000)

    handleNotif :: Notification -> IO ()
    handleNotif notif@Notification {..} = do
        logMsg LogInfo
            $  "Got update from Decks frontend:\n"
            <> (T.pack . show) notif

        case notifType of
            NotifTextChanged -> docEditTextRange dslPath notifSrc notifNewVal

--------------------------------------------------------------------------------
