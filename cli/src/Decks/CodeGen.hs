--------------------------------------------------------------------------------

-- | Generates HTML code from the Decks parse tree.
--
module Decks.CodeGen where

import           Decks.CodeGen.Generate
import           Decks.CodeGen.Types
import           Decks.Error
import           Decks.Grammar                  ( DecksProgram(..) )
import           Decks.Logging

import           Control.Monad.Trans.State

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

--------------------------------------------------------------------------------

outPath :: FilePath
outPath = "presentation.html"

-- | Runs the code generation, logging the successful result or error.
runCodeGen :: DecksProgram -> IO ()
runCodeGen p = case evalStateT (genProgram p) initDecksStore of
    Left  err -> logMsg LogError $ showCodeGenErr err
    Right res -> do
        logMsg LogSuccess "Generated HTML output successfully"
        TIO.putStrLn res
        logMsg LogInfo $ "Writing output to " <> T.pack outPath
        TIO.writeFile outPath res

--------------------------------------------------------------------------------
