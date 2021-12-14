--------------------------------------------------------------------------------

-- | Generates HTML code from the Decks parse tree.
--
module Decks.CodeGen where

import           Decks.Grammar
import           Decks.Logging

import           Control.Monad.Trans.State

import           Data.Functor
import qualified Data.HashMap.Lazy             as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO


--------------------------------------------------------------------------------
-- Error types and utility functions
--------------------------------------------------------------------------------

data CodeGenError = UndefinedIdentErr Identifier
    deriving Eq

showCodeGenErr :: CodeGenError -> Text
showCodeGenErr (UndefinedIdentErr i) =
    T.concat ["Undefined identifier '", unIdentifier i, "'"]


--------------------------------------------------------------------------------
-- Types and state
--------------------------------------------------------------------------------

type Html = Text
type HtmlResult = Either CodeGenError Html

-- type ConstantsMap = M.HashMap Identifier DecksElement -- TODO:
type DefinitionMap = M.HashMap Identifier ContentTemplate

data DecksState = DecksState
    { stDefinitions :: DefinitionMap
    }

initDecksState :: DecksState
initDecksState = DecksState M.empty

insertDef :: Identifier -> ContentTemplate -> DecksState -> DecksState
insertDef i ct (DecksState defs) = DecksState $ M.insert i ct defs

getIdentifier :: Identifier -> DecksState -> Maybe ContentTemplate
getIdentifier i DecksState {..} = M.lookup i stDefinitions


--------------------------------------------------------------------------------
-- Main runner
--------------------------------------------------------------------------------

-- | Runs the code generation, logging the successful result or error.
runCodeGen :: DecksProgram -> IO ()
runCodeGen p = case evalState (genProgram p) initDecksState of
    Left  err -> logMsg LogError $ showCodeGenErr err
    Right res -> do
        logMsg LogSuccess "Generated HTML output successfully"
        TIO.putStrLn res


--------------------------------------------------------------------------------
-- Code generation
--------------------------------------------------------------------------------

-- | Returns either successfully generated HTML from the Decks AST, or an error.
genProgram :: DecksProgram -> State DecksState HtmlResult
genProgram (DecksProgram stmts) = mapM genStmt stmts <&> combineResults
    where combineResults = fmap T.concat . sequenceA

-- | Generates HTML output from a Decks statement.
genStmt :: DecksStmt -> State DecksState HtmlResult
genStmt (DecksDrawStmt el ) = genElement el
genStmt (DecksDefStmt i ct) = withState (insertDef i ct) (pure $ Right mempty)
genStmt _                   = error "Not implemented"

genElement :: DecksElement -> State DecksState HtmlResult
-- FIXME: include attributes and classes
genElement (DecksElement i _ _) = gets (getIdentifier i) <&> \case
    Nothing -> Left $ UndefinedIdentErr i
    Just ct -> Right $ unContentTemplate ct

--------------------------------------------------------------------------------
