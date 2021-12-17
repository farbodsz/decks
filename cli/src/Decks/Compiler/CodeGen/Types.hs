--------------------------------------------------------------------------------

-- | Types and utility functions for code generation.
--
module Decks.Compiler.CodeGen.Types where

import           Decks.Compiler.Error
import           Decks.Compiler.Grammar

import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State

import qualified Data.HashMap.Lazy             as M
import           Data.List                      ( union )
import           Data.Maybe
import           Data.Text                      ( Text )

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
    , pctContent  :: Html
    -- ^ Content to be applied to the template.
    }

updatePct
    :: PendingContentTemplate     -- ^ Initial 'PendingContentTemplate'.
    -> [DecksAttr]                -- ^ Attributes overriding the initial ones.
    -> Html                       -- ^ Content overriding the initial one.
    -> PendingContentTemplate     -- ^ Resulting 'PendingContentTemplate'.
updatePct PendingContentTemplate {..} as =
    PendingContentTemplate pctTemplate (as `union` pctAttrs)

-- | The (pending) content represented by each identifier.
type VariableMap = M.HashMap Identifier PendingContentTemplate

-- | Number of usages for each identifier.
type UsageMap = M.HashMap Identifier Int

data DecksStore = DecksStore
    { stDefs   :: VariableMap
    , stLets   :: VariableMap
    , stUsages :: UsageMap
    }

initDecksStore :: DecksStore
initDecksStore = DecksStore M.empty M.empty M.empty

insertDef :: Identifier -> ContentTemplate -> DecksStore -> DecksStore
insertDef i ct DecksStore {..} = DecksStore (M.insert i pct stDefs)
                                            stLets
                                            (M.insert i 0 stUsages)
    where pct = PendingContentTemplate ct [] mempty

insertLet :: Identifier -> PendingContentTemplate -> DecksStore -> DecksStore
insertLet i t DecksStore {..} =
    DecksStore stDefs (M.insert i t stLets) (M.insert i 0 stUsages)

-- | Marks the usage of the given identifier.
markUsage :: Identifier -> DecksStore -> DecksStore
markUsage i DecksStore {..} = DecksStore stDefs stLets updatedUsages
    where updatedUsages = M.insertWith (\_ old -> old + 1) i 1 stUsages

-- | Returns True if the identifier has already been defined in a prior program
-- statement.
alreadyDefined :: Identifier -> DecksStore -> Bool
alreadyDefined i DecksStore {..} = M.member i stDefs || M.member i stLets

-- | Lookups up an identifier from the map of existing declarations.
lookupIdentifier :: Identifier -> DecksStore -> Maybe PendingContentTemplate
lookupIdentifier i DecksStore {..} = mapMaybeFirst (M.lookup i) srcs
  where
    mapMaybeFirst f = listToMaybe . mapMaybe f
    srcs = [stDefs, stLets]

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
