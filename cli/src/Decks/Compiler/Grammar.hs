--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Compiler.Grammar where

import           Control.Applicative            ( (<|>) )
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( union )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Decks.Document                 ( SrcRange )

--------------------------------------------------------------------------------

-- | The Parse Syntax Tree for the Decks program.
newtype DecksProgram = DecksProgram [DecksStmt]
    deriving Show

data DecksStmt
    = DecksDrawStmt SrcRange DecksElement
    | DecksLetStmt
        { letIdent :: Identifier
        , letElem  :: DecksElement
        }
    | DecksDefStmt
        { defIdent           :: Identifier
        , defContentTemplate :: ContentTemplate
        }
    | DecksString SrcRange Text
    | DecksLiteral Text
    | DecksComment
        { commentText :: Text
        }
    deriving (Eq, Show)

-- | Identifies a drawable element.
newtype Identifier = Identifier { unIdentifier :: Text }
    deriving (Eq, Hashable, Show)

newtype ContentTemplate = ContentTemplate { unContentTemplate :: Text }
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent :: Identifier
    , elProps :: DecksElemProps
    , elStmts :: [DecksStmt]
    }
    deriving (Eq, Show)

-- | Decks elements can have attributes attached to them.
--
-- While these can be generated into HTML, Decks attributes have different
-- syntax to HTML attributes, so they should not be confused.
--
data DecksElemProps = DecksElemProps
    { propsId      :: Maybe Text              -- ^ Decks syntax: @#identifier@
    , propsClasses :: [Text]                  -- ^ Decks syntax: @.class-name@
    , propsStyles  :: M.Map Text Text         -- ^ Decks syntax: @%key="val"@
    , propsAttrs   :: M.Map Text (Maybe Text) -- ^ Decks syntax: @attr="val"@
    }
    deriving (Eq, Show)

instance Semigroup DecksElemProps where
    p1 <> p2 = DecksElemProps (propsId p2 <|> propsId p1)
                              (propsClasses p2 `union` propsClasses p1)
                              (propsStyles p2 `M.union` propsStyles p1)
                              (propsAttrs p2 `M.union` propsAttrs p1)

instance Monoid DecksElemProps where
    mempty = DecksElemProps mempty mempty mempty mempty

-- | Adds the given attribute to the current props.
addElemAttrs :: [(Text, Maybe Text)] -> DecksElemProps -> DecksElemProps
addElemAttrs as ps = ps { propsAttrs = M.fromList as `M.union` propsAttrs ps }

--------------------------------------------------------------------------------
