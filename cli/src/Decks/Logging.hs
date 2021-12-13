--------------------------------------------------------------------------------

-- | Provides logging functionality.
--
module Decks.Logging where

import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Time.Format
import           System.FSNotify                ( Event(..) )
import           System.FilePath.Posix          ( takeFileName )

--------------------------------------------------------------------------------

logEvent :: Event -> IO ()
logEvent (Modified file time _) =
    logInfo time $ "Detected change in " ++ takeFileName file
logEvent _ = getCurrentTime >>= \time -> logInfo time "Unknown event"

logInfo :: UTCTime -> String -> IO ()
logInfo time msg =
    putStrLn
        $  "[INFO] "
        ++ formatTime defaultTimeLocale "%H:%M:%S" time
        ++ ": "
        ++ msg

--------------------------------------------------------------------------------
