--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Compiler.Grammar where

import           Data.Hashable                  ( Hashable )
import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

-- | The Parse Syntax Tree for the Decks program.
newtype DecksProgram = DecksProgram [DecksStmt]

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
    , elAttrs :: [DecksAttr]
    , elStmts :: [DecksStmt]
    }
    deriving (Eq, Show)

-- | Elements can have attributes attached to them, referring to external CSS
-- code.
data DecksAttr
    = CssId Text              -- ^ E.g. @#identifier@
    | CssClass Text           -- ^ E.g. @.class-name@
    | CssStyle                -- ^ E.g. @key="val"@
        { cssStyKey :: Text
        , cssStyVal :: Text
        }
    | HtmlAttr                -- ^ E.g. @attr@
        { htmlAttrName :: Text
        }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
