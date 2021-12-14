--------------------------------------------------------------------------------

-- | Grammar of the Decks DSL syntax.
--
module Decks.Grammar where

import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

-- | The Parse Syntax Tree for the Decks program.
newtype DecksProgram = DecksProgram [DecksStmt]

-- | Identifies a drawable element.
newtype Identifier = Identifier Text
    deriving (Eq, Show)

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

type ContentTemplate = Text

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent   :: Identifier
    , elAttrs   :: [DecksAttr]
    , elContent :: Maybe Content
    }
    deriving (Eq, Show)

type Content = Text

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
