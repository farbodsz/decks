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
    | LogError

logLevelTag :: LogLevel -> Text
logLevelTag LogInfo  = "INFO "
logLevelTag LogError = "ERROR"

logLevelColor :: LogLevel -> Color
logLevelColor LogInfo  = Cyan
logLevelColor LogError = Red

--------------------------------------------------------------------------------

logEvent :: Event -> IO ()
logEvent (Modified file time _) =
    logMsg LogInfo time $ "Detected change in " <> T.pack (takeFileName file)
logEvent _ = getCurrentTime >>= \time -> logMsg LogError time "Unknown event"

logMsg :: LogLevel -> UTCTime -> Text -> IO ()
logMsg lvl time msg = do
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
