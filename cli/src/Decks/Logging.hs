--------------------------------------------------------------------------------

-- | Provides logging functionality.
--
module Decks.Logging where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format
import           System.Console.ANSI
import           System.FSNotify                ( Event(..) )
import           System.FilePath.Posix          ( takeFileName )

--------------------------------------------------------------------------------

data LogLevel
    = LogInfo
    | LogWarn
    | LogError
    | LogSuccess

logLevelTag :: LogLevel -> Text
logLevelTag LogInfo    = "INFO "
logLevelTag LogWarn    = "WARN "
logLevelTag LogError   = "ERROR"
logLevelTag LogSuccess = "DONE "

logLevelColor :: LogLevel -> Color
logLevelColor LogInfo    = Cyan
logLevelColor LogWarn    = Yellow
logLevelColor LogError   = Red
logLevelColor LogSuccess = Green

--------------------------------------------------------------------------------

logEvent :: Event -> IO ()
logEvent (Modified file time _) =
    logMsgAt time LogInfo $ "Detected change in " <> T.pack (takeFileName file)
logEvent _ = logMsg LogError "Unknown event"

--------------------------------------------------------------------------------

logMsg :: LogLevel -> Text -> IO ()
logMsg lvl msg = getCurrentTime >>= \time -> logMsgAt time lvl msg

logMsgAt :: UTCTime -> LogLevel -> Text -> IO ()
logMsgAt time lvl msg = do
    setSGR [SetColor Foreground Vivid (logLevelColor lvl)]
    TIO.putStrLn
        $  "["
        <> logLevelTag lvl
        <> "] "
        <> T.pack (formatTime defaultTimeLocale "%H:%M:%S" time)
        <> ": "
        <> msg
    setSGR [Reset]

--------------------------------------------------------------------------------
