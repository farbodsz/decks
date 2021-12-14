--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Grammar where

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
    deriving (Eq, Show)

newtype ContentTemplate = ContentTemplate { unContentTemplate :: Text }
    deriving (Eq, Show)

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent   :: Identifier
    , elAttrs   :: [DecksAttr]
    , elContent :: Maybe Content
    }
    deriving (Eq, Show)

newtype Content = Content { unContent :: Text }
    deriving (Eq, Show)

-- | Elements can have attributes attached to them, referring to external CSS
-- code.
data DecksAttr
    = CssId Text              -- ^ E.g. @#identifier@
    | CssClass Text           -- ^ E.g. @.class-name@
    | CssProp                 -- ^ E.g. @key="val"@
        { cssPropKey :: Text
        , cssPropVal :: Text
        }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
