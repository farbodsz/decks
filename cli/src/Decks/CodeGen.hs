--------------------------------------------------------------------------------

-- | Generates HTML code from the Decks parse tree.
--
module Decks.CodeGen where

import           Decks.CodeGen.Generate
import           Decks.CodeGen.Types
import           Decks.Error
import           Decks.Grammar                  ( DecksProgram(..)
                                                , Identifier(..)
                                                )
import           Decks.Logging

import           Control.Monad                  ( forM_ )
import           Control.Monad.Trans.State

import qualified Data.HashMap.Lazy             as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

--------------------------------------------------------------------------------

outPath :: FilePath
outPath = "presentation.html"

-- | Runs the code generation, logging the successful result or error.
runCodeGen :: DecksProgram -> IO ()
runCodeGen p = case runStateT (genProgram p) initDecksStore of
    Left  err           -> logMsg LogError $ showCodeGenErr err
    Right (html, store) -> do
        generateWarnings store
        logMsg LogSuccess "Generated HTML output successfully"
        TIO.putStrLn html
        logMsg LogInfo $ "Writing output to " <> T.pack outPath
        TIO.writeFile outPath html

generateWarnings :: DecksStore -> IO ()
generateWarnings store =
    let unusedIdents = M.keys $ M.filter (== 0) (stUsages store)
    in  forM_ unusedIdents $ \i ->
            logMsg LogWarn $ "Unused identifier '" <> unIdentifier i <> "'"

--------------------------------------------------------------------------------
