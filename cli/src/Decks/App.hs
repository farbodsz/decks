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
    opts <- parseCmd
    watch opts

-- | watch @directory shouldWatch@ continuously watches for Decks files,
-- updating as they are modified, if @shouldWatch@ is True. Otherwise, the
-- file(s) are only read once.
watch :: Opts -> IO ()
watch Opts {..} = if not optWatch
    then getDecksFromDir optDirPath >>= mapM_ (compile optOutPath optVerbose)
    else withManager $ \mgr -> do
        logMsg LogInfo $ "Watching directory " <> T.pack optDirPath

        -- Process once before watching for further changes in the background
        getDecksFromDir optDirPath >>= mapM_ (compile optOutPath optVerbose)
        _ <- watchDir mgr
                      optDirPath
                      shouldCheckFile
                      (processEvent optDirPath optOutPath optVerbose)

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

processEvent :: FilePath -> FilePath -> Bool -> Event -> IO ()
processEvent _ outPath verbose ev@(Modified file _ _) = do
    logEvent ev
    compile outPath verbose file
processEvent _ _ _ _ = pure ()

--------------------------------------------------------------------------------
