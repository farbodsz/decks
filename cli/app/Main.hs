--------------------------------------------------------------------------------

module Main where

import           Decks.App
import           Decks.Parser                   ( parseDecks )

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Data.Time.Clock
import           Data.Time.Format
import           System.FSNotify
import           System.FilePath

--------------------------------------------------------------------------------

main :: IO ()
main = do
    Opts {..} <- parseCmd
    watch optFilepath

watch :: FilePath -> IO ()
watch path = withManager $ \mgr -> do
    time <- getCurrentTime
    logInfo time $ "Watching directory " ++ path

    -- Start a watching job (in the background)
    _ <- watchDir mgr path shouldCheckFile (processEvent path)

    -- Sleep forever (until interrupted)
    forever $ threadDelay 1000000

shouldCheckFile :: Event -> Bool
shouldCheckFile (Modified file _ _) = takeExtension file == ".decks"
shouldCheckFile _                   = False

processEvent :: FilePath -> Event -> IO ()
processEvent _ ev@(Modified file _ _) = logEvent ev >>= pure (parseDecks file)
processEvent _ _                      = pure ()

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
