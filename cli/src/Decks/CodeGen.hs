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

data CodeGenError
    = UndefinedIdentifier Identifier
    | EmptyContent
    | MultipleCssIds
    | InternalError Text
    deriving Eq

showCodeGenErr :: CodeGenError -> Text
showCodeGenErr (UndefinedIdentifier i) =
    T.concat ["Undefined identifier '", unIdentifier i, "'"]
showCodeGenErr EmptyContent        = "Element content cannot be empty"
showCodeGenErr MultipleCssIds      = "HTML ID attributes must be unique"
showCodeGenErr (InternalError msg) = "Internal error: " <> msg


--------------------------------------------------------------------------------
-- Types and state
--------------------------------------------------------------------------------

type Html = Text
type HtmlResult = Either CodeGenError Html

type DefinitionMap = M.HashMap Identifier ContentTemplate
-- type VariableMap = M.HashMap Identifier DecksElement -- TODO:

data DecksState = DecksState
    { stDefinitions :: DefinitionMap
    -- , stVariables :: VariableMap
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
genStmt _                   = pure . Left . InternalError $ "Not implemented"

genElement :: DecksElement -> State DecksState HtmlResult
genElement (DecksElement i as mc) = gets (getIdentifier i) <&> \case
    Nothing -> Left $ UndefinedIdentifier i
    Just ct -> fillContentTemplate as mc ct

-- | Returns the HTML representing a filled-in content template.
fillContentTemplate
    :: [DecksAttr] -> Maybe Content -> ContentTemplate -> HtmlResult
fillContentTemplate as mc (ContentTemplate ct) = do
    attrs   <- fillCtAttrs as
    content <- maybe (Left EmptyContent) (Right . unContent) mc
    pure . T.replace "$style$" attrs . T.replace "$content$" content $ ct

data AttrHtmlResults = AttrHtmlResults
    { attrIdent   :: HtmlResult
    , attrClasses :: HtmlResult
    , attrStyles  :: HtmlResult
    }

-- | Returns the HTML attributes text corresponding to the Decks attributes,
-- i.e. HTML @id@, @class@, and @style@ attributes.
fillCtAttrs :: [DecksAttr] -> HtmlResult
fillCtAttrs = attrsToHtml . processAttributes
  where
    attrsToHtml :: AttrHtmlResults -> HtmlResult
    attrsToHtml AttrHtmlResults {..} =
        fmap (T.strip . T.unwords)
            . sequenceA
            $ [attrIdent, attrClasses, attrStyles]

    processAttributes :: [DecksAttr] -> AttrHtmlResults
    processAttributes as =
        let isId (CssId _) = True
            isId _         = False
            isClass (CssClass _) = True
            isClass _            = False
            isStyle (CssProp _ _) = True
            isStyle _             = False
            process converter predicate = converter . filter predicate $ as
        in  AttrHtmlResults { attrIdent   = process idsToHtml isId
                            , attrClasses = process classesToHtml isClass
                            , attrStyles  = process stylesToHtml isStyle
                            }

    idsToHtml :: [DecksAttr] -> HtmlResult
    idsToHtml []        = Right ""
    idsToHtml [CssId i] = Right $ "id=\"" <> i <> "\""
    idsToHtml _         = Left MultipleCssIds

    classesToHtml :: [DecksAttr] -> HtmlResult
    classesToHtml [] = Right ""
    classesToHtml cs =
        let clsNames = T.unwords $ map (\(CssClass name) -> "." <> name) cs
        in  Right $ "class=\"" <> clsNames <> "\""

    -- TODO: Disallow double quotes in attributes
    stylesToHtml :: [DecksAttr] -> HtmlResult
    stylesToHtml [] = Right ""
    stylesToHtml ps =
        let kvTexts = T.unwords $ map (\(CssProp k v) -> k <> ":" <> v) ps
        in  Right $ "style=\"" <> kvTexts <> "\""

--------------------------------------------------------------------------------
