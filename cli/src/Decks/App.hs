--------------------------------------------------------------------------------

-- | The app simply reads the parsed CLI commands and runs computuations based
-- on them.
--
module Decks.App where

import           Decks.Commands
import           Decks.Logging
import           Decks.Parser                   ( parseDecks )

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )
import           Data.Time.Clock                ( getCurrentTime )
import           System.FSNotify
import           System.FilePath                ( takeExtension )

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
