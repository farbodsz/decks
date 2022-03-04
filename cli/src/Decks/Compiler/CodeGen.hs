--------------------------------------------------------------------------------

-- | Generates HTML code from the Decks parse tree.
--
module Decks.Compiler.CodeGen where

import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Control.Monad.Trans.State
import qualified Data.HashMap.Lazy             as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Decks.Compiler.CodeGen.Generate
import           Decks.Compiler.CodeGen.Types
import           Decks.Compiler.Error
import           Decks.Compiler.Grammar         ( DecksProgram(..)
                                                , Identifier(..)
                                                )
import           Decks.Document                 ( HtmlOutput(HtmlOutput) )
import           Decks.Logging

--------------------------------------------------------------------------------

-- | Runs the code generation, logging the successful result or error.
runCodeGen
    :: HtmlOutput       -- ^ Output file
    -> Bool             -- ^ Verbose?
    -> DecksProgram     -- ^ AST
    -> IO ()
runCodeGen (HtmlOutput outPath) verbose p =
    case runStateT (genProgram p) initDecksStore of
        Left  err           -> logMsg LogError $ showCodeGenErr err
        Right (html, store) -> do
            generateWarnings store
            logMsg LogSuccess "Generated HTML output successfully"
            when verbose $ do
                TIO.putStrLn html
                logMsg LogInfo $ "Writing output to " <> T.pack outPath
            TIO.writeFile outPath html

generateWarnings :: DecksStore -> IO ()
generateWarnings store =
    let unusedIdents = M.keys $ M.filter (== 0) (stUsages store)
    in  forM_ unusedIdents $ \i ->
            logMsg LogWarn $ "Unused identifier '" <> unIdentifier i <> "'"

--------------------------------------------------------------------------------
