--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Compiler.Grammar where

import           Data.Hashable                  ( Hashable )
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
    , elProps :: [DecksElemProp]
    , elStmts :: [DecksStmt]
    }
    deriving (Eq, Show)

-- | Decks elements can have attributes attached to them.
--
-- While these can be generated into HTML, Decks attributes have different
-- syntax to HTML attributes, so they should not be confused.
--
data DecksElemProp
    = ElemPropId Text               -- ^ Decks syntax: @#identifier@
    | ElemPropClass Text            -- ^ Decks syntax: @.class-name@
    | ElemPropStyle                 -- ^ Decks syntax: @key="val"@
        { propStyKey :: Text
        , propStyVal :: Text
        }
    | ElemPropAttr                  -- ^ Decks syntax: @attr@
        { propAttrName :: Text
        }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
