--------------------------------------------------------------------------------

-- | The app simply reads the parsed CLI commands and runs computuations based
-- on them.
--
module Decks.App where

import           Decks.Commands
import           Decks.Logging

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )

import qualified Data.Text                     as T

import           Decks.Compile                  ( compile )
import           System.Directory
import           System.FSNotify
import           System.FilePath                ( takeExtension )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    Opts {..} <- parseCmd
    watch optFilepath optWatch

-- | watch @directory shouldWatch@ continuously watches for Decks files,
-- updating as they are modified, if @shouldWatch@ is True. Otherwise, the
-- file(s) are only read once.
watch :: FilePath -> Bool -> IO ()
watch path False = getDecksFromDir path >>= mapM_ compile
watch path True  = withManager $ \mgr -> do
    logMsg LogInfo $ "Watching directory " <> T.pack path

    -- Process once before watching for further changes in the background
    getDecksFromDir path >>= mapM_ compile
    _ <- watchDir mgr path shouldCheckFile (processEvent path)

    -- Sleep forever (until interrupted)
    forever $ threadDelay 1000000

-- | A list of Decks files in the directory.
getDecksFromDir :: FilePath -> IO [FilePath]
getDecksFromDir = fmap (filter isDecksFile) . getDirectoryContents

shouldCheckFile :: Event -> Bool
shouldCheckFile (Modified file _ _) = isDecksFile file
shouldCheckFile _                   = False

isDecksFile :: FilePath -> Bool
isDecksFile fp = takeExtension fp == ".decks"

processEvent :: FilePath -> Event -> IO ()
processEvent _ ev@(Modified file _ _) = logEvent ev >> compile file
processEvent _ _                      = pure ()

--------------------------------------------------------------------------------
