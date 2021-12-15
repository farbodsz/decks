--------------------------------------------------------------------------------

-- | Defines custom error types and error handling.
--
module Decks.Error where

import           Decks.Grammar

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

data CodeGenError
    = UndefinedIdentifier Identifier
    | MultipleDefinitions Identifier
    | EmptyContent
    | MultipleCssIds
    | InternalError Text
    deriving Eq

showCodeGenErr :: CodeGenError -> Text
showCodeGenErr (UndefinedIdentifier i) =
    T.concat ["Undefined identifier '", unIdentifier i, "'"]
showCodeGenErr (MultipleDefinitions i) =
    T.concat ["Identifier '", unIdentifier i, "' already defined."]
showCodeGenErr EmptyContent        = "Element content cannot be empty"
showCodeGenErr MultipleCssIds      = "HTML ID attributes must be unique"
showCodeGenErr (InternalError msg) = "Internal error: " <> msg

--------------------------------------------------------------------------------
