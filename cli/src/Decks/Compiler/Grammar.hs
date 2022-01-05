--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Compiler.Grammar where

import           Control.Applicative            ( (<|>) )
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( union )
import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

-- | The Parse Syntax Tree for the Decks program.
newtype DecksProgram = DecksProgram [DecksStmt]
    deriving Show

-- | Identifies a drawable element.
newtype Identifier = Identifier { unIdentifier :: Text }
    deriving (Eq, Hashable, Show)

data DecksStmt
    = DecksDrawStmt
        { drawElem :: DecksElement
        }
    | DecksLetStmt
        { letIdent :: Identifier
        , letElem  :: DecksElement
        }
    | DecksDefStmt
        { defIdent           :: Identifier
        , defContentTemplate :: ContentTemplate
        }
    | DecksLiteral
        { litContent :: Text
        }
    | DecksComment
        { commentText :: Text
        }
    deriving (Eq, Show)

newtype ContentTemplate = ContentTemplate { unContentTemplate :: Text }
    deriving (Eq, Show)

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
    { propId      :: Maybe Text             -- ^ Decks syntax: @#identifier@
    , propClasses :: [Text]                 -- ^ Decks syntax: @.class-name@
    , propStyles  :: [(Text, Text)]         -- ^ Decks syntax: @%key="val"@
    , propAttrs   :: [(Text, Maybe Text)]   -- ^ Decks syntax: @attr="val"@
    }
    deriving (Eq, Show)

instance Semigroup DecksElemProps where
    p1 <> p2 = DecksElemProps (propId p1 <|> propId p2)
                              (propClasses p1 `union` propClasses p2)
                              (propStyles p1 `union` propStyles p2)
                              (propAttrs p1 `union` propAttrs p2)

instance Monoid DecksElemProps where
    mempty = DecksElemProps Nothing [] [] []

--------------------------------------------------------------------------------
