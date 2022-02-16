--------------------------------------------------------------------------------

-- | The app simply reads the parsed CLI commands and runs computuations based
-- on them.
--
module Decks.App where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Monad                  ( forever )
import qualified Data.Text                     as T
import           Decks.Commands
import           Decks.Compiler                 ( compile )
import           Decks.Document                 ( DecksDocument(..)
                                                , HtmlOutput(HtmlOutput)
                                                )
import           Decks.Logging
import           Decks.Server                   ( runServer )
import           System.Directory
import           System.FSNotify
import           System.FilePath                ( takeDirectory
                                                , takeExtension
                                                )

--------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- parseCmd
    concurrently_
        (runServer (HtmlOutput $ optOutPath opts) (optFrontendUrl opts))
        (watch opts)

-- | watch @directory shouldWatch@ continuously watches for Decks files,
-- updating as they are modified, if @shouldWatch@ is True. Otherwise, the
-- file(s) are only read once.
watch :: Opts -> IO ()
watch Opts {..} = if not optWatch
    then compile outPath optVerbose dslPath
    else withManager $ \mgr -> do
        let dirPath = takeDirectory optDslPath
        logMsg LogInfo $ "Watching directory " <> T.pack dirPath

        -- Process once before watching for further changes in the background
        compile outPath optVerbose dslPath
        _ <- watchDir mgr
                      dirPath
                      shouldCheckFile
                      (processEvent dirPath dslPath outPath optVerbose)

        -- Sleep forever (until interrupted)
        forever $ threadDelay 1000000
  where
    dslPath = DecksDocument optDslPath
    outPath = HtmlOutput optOutPath

-- | A list of Decks files in the directory.
getDecksFromDir :: FilePath -> IO [DecksDocument]
getDecksFromDir =
    fmap (map DecksDocument . filter isDecksFile) . getDirectoryContents

shouldCheckFile :: Event -> Bool
shouldCheckFile (Modified file _ _) = isDecksFile file
shouldCheckFile _                   = False

isDecksFile :: FilePath -> Bool
isDecksFile fp = takeExtension fp == ".decks"

processEvent
    :: FilePath -> DecksDocument -> HtmlOutput -> Bool -> Event -> IO ()
processEvent _dirPath _dslPath outPath verbose ev@(Modified file _ _) = do
    -- TODO: check if the file that has changed is the one we're watching
    logEvent ev
    compile outPath verbose (DecksDocument file)
processEvent _ _ _ _ _ = pure ()

--------------------------------------------------------------------------------
