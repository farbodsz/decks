--------------------------------------------------------------------------------

-- | Defines custom error types and error handling.
--
module Decks.Compiler.Error where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Decks.Compiler.Grammar         ( Identifier(..) )

--------------------------------------------------------------------------------

data CodeGenError
    = UndefinedIdentifier Identifier
    | MultipleDefinitions Identifier
    | EmptyContent
    | MultipleElemPropIds
    | InternalError Text
    deriving Eq

showCodeGenErr :: CodeGenError -> Text
showCodeGenErr (UndefinedIdentifier i) =
    T.concat ["Undefined identifier '", unIdentifier i, "'"]
showCodeGenErr (MultipleDefinitions i) =
    T.concat ["Identifier '", unIdentifier i, "' already defined."]
showCodeGenErr EmptyContent        = "Element content cannot be empty"
showCodeGenErr MultipleElemPropIds = "HTML ID attributes must be unique"
showCodeGenErr (InternalError msg) = "Internal error: " <> msg

--------------------------------------------------------------------------------
