--------------------------------------------------------------------------------

-- | Generates HTML code from the Decks parse tree.
--
module Decks.CodeGen where

import           Decks.Error
import           Decks.Grammar
import           Decks.Logging

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State

import           Data.Functor                   ( (<&>) )
import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( union )
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO


--------------------------------------------------------------------------------
-- Types and state
--------------------------------------------------------------------------------

type Html = Text
type HtmlResult = Either CodeGenError Html

-- | Represents a content template with some default HTML attributes, used for
-- let bindings, awaiting conversion to a HTML result.
--
-- Let bindings essentially bind an identifier to an existing definition,
-- possibly with some attributes set. For example, in the following Decks
-- program, example refers to a type of text with certain attributes and
-- content:
-- @
-- !def text = { <p $attrs$>$content$</p> }
-- !let example = text [.my-class] { Hello World! }
-- example
-- example [.overridden-style] { Overridden content }
-- @
--
-- The initial attributes cannot be stored as HTML, its final stage, since they
-- can still be overridden (L4 of example), hence @DecksAttr@ from the grammar
-- is used.
--
data PendingContentTemplate = PendingContentTemplate
    { pctTemplate :: ContentTemplate
    -- ^ The template text with template strings like @$content$@..
    , pctAttrs    :: [DecksAttr]
    -- ^ Attributes waiting to be applied to the template.
    , pctContent  :: Maybe Content
    -- ^ Content waiting to be applied to the template.
    }

updatePct
    :: PendingContentTemplate     -- ^ Initial 'PendingContentTemplate'.
    -> [DecksAttr]                -- ^ Attributes overriding the initial ones.
    -> Maybe Content              -- ^ Content overriding the initial one.
    -> PendingContentTemplate     -- ^ Resulting 'PendingContentTemplate'.
updatePct PendingContentTemplate {..} as mc =
    PendingContentTemplate pctTemplate (as `union` pctAttrs) (mc <|> pctContent)

type VariableMap = M.HashMap Identifier PendingContentTemplate

data DecksStore = DecksStore
    { stDefinitions :: VariableMap
    , stLetBindings :: VariableMap
    }

initDecksStore :: DecksStore
initDecksStore = DecksStore M.empty M.empty

insertDef :: Identifier -> ContentTemplate -> DecksStore -> DecksStore
insertDef i ct (DecksStore defs lets) = DecksStore (M.insert i pct defs) lets
    where pct = PendingContentTemplate ct [] Nothing

insertLet :: Identifier -> PendingContentTemplate -> DecksStore -> DecksStore
insertLet i t (DecksStore defs lets) = DecksStore defs (M.insert i t lets)

-- | Returns True if the identifier has already been defined in a prior program
-- statement.
alreadyDefined :: Identifier -> DecksStore -> Bool
alreadyDefined i (DecksStore defs lets) = M.member i defs || M.member i lets

-- | Lookups up an identifier from the map of existing declarations.
lookupIdentifier :: Identifier -> DecksStore -> Maybe PendingContentTemplate
lookupIdentifier i (DecksStore defs lets) = mapMaybeFirst (M.lookup i) srcs
  where
    mapMaybeFirst f = listToMaybe . mapMaybe f
    srcs = [defs, lets]


--------------------------------------------------------------------------------
-- Decks monad stack
--------------------------------------------------------------------------------

type DecksM = StateT DecksStore (Either CodeGenError)

-- | Retrieves the pending content template of the given identifier.
getIdentPct :: Identifier -> DecksM PendingContentTemplate
getIdentPct i = gets (lookupIdentifier i) >>= \case
    Nothing  -> lift $ Left $ UndefinedIdentifier i
    Just pct -> lift $ Right pct

-- | 'getUniqIdent' @identifier@ returns the identifier if not already declared,
-- otherwise an error.
getUniqIdent :: Identifier -> DecksM Identifier
getUniqIdent ident = get >>= \store -> lift $ if alreadyDefined ident store
    then Left $ MultipleDefinitions ident
    else Right ident


--------------------------------------------------------------------------------
-- Main runner
--------------------------------------------------------------------------------

-- | Runs the code generation, logging the successful result or error.
runCodeGen :: DecksProgram -> IO ()
runCodeGen p = case evalStateT (genProgram p) initDecksStore of
    Left  err -> logMsg LogError $ showCodeGenErr err
    Right res -> do
        logMsg LogSuccess "Generated HTML output successfully"
        TIO.putStrLn res


--------------------------------------------------------------------------------
-- Code generation
--------------------------------------------------------------------------------

-- | Returns either successfully generated HTML from the Decks AST, or an error.
genProgram :: DecksProgram -> DecksM Html
genProgram (DecksProgram stmts) = mapM genStmt stmts <&> T.concat

-- | Generates HTML output from a Decks statement.
genStmt :: DecksStmt -> DecksM Html
genStmt (DecksDrawStmt el ) = genElement el
genStmt (DecksDefStmt i ct) = do
    ident <- getUniqIdent i
    withStateT (insertDef ident ct) (pure mempty)
genStmt (DecksLetStmt i DecksElement {..}) = do
    ident <- getUniqIdent i
    pct   <- getIdentPct elIdent
    withStateT (insertLet ident (updatePct pct elAttrs elContent)) (pure mempty)

genElement :: DecksElement -> DecksM Html
genElement DecksElement {..} = do
    pct <- getIdentPct elIdent
    lift $ fillContentTemplate $ updatePct pct elAttrs elContent

-- | Returns the HTML representing a filled-in content template.
fillContentTemplate :: PendingContentTemplate -> HtmlResult
fillContentTemplate (PendingContentTemplate (ContentTemplate ct) as mc) = do
    attrs   <- fillCtAttrs as
    content <- maybe (Left EmptyContent) (Right . unContent) mc
    pure . T.replace "$attrs$" attrs . T.replace "$content$" content $ ct

data HtmlAttributes = HtmlAttributes
    { attrIdent   :: HtmlResult
    , attrClasses :: HtmlResult
    , attrStyles  :: HtmlResult
    }

-- | Returns the HTML attributes text corresponding to the Decks attributes,
-- i.e. HTML @id@, @class@, and @style@ attributes.
fillCtAttrs :: [DecksAttr] -> HtmlResult
fillCtAttrs = attrsToHtml . processAttributes
  where
    attrsToHtml :: HtmlAttributes -> HtmlResult
    attrsToHtml HtmlAttributes {..} =
        fmap (T.strip . T.unwords)
            . sequenceA
            $ [attrIdent, attrClasses, attrStyles]

    processAttributes :: [DecksAttr] -> HtmlAttributes
    processAttributes as =
        let isId (CssId _) = True
            isId _         = False
            isClass (CssClass _) = True
            isClass _            = False
            isStyle (CssProp _ _) = True
            isStyle _             = False
            process converter predicate = converter . filter predicate $ as
        in  HtmlAttributes { attrIdent   = process idsToHtml isId
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

    stylesToHtml :: [DecksAttr] -> HtmlResult
    stylesToHtml [] = Right ""
    stylesToHtml ps =
        let kvTexts = T.unwords $ map (\(CssProp k v) -> k <> ":" <> v) ps
        in  Right $ "style=\"" <> kvTexts <> "\""

--------------------------------------------------------------------------------
