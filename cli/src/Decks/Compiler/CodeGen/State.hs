--------------------------------------------------------------------------------

-- | State used throughout code generation.
--
module Decks.Compiler.CodeGen.State where

import qualified Data.HashMap.Lazy             as M
import           Data.Stack                     ( Stack )

import           Decks.Compiler.CodeGen.Types   ( PendingContentTemplate )
import           Decks.Compiler.Grammar         ( Identifier )

--------------------------------------------------------------------------------

-- | The (pending) content represented by each identifier.
type VariableMap = M.HashMap Identifier PendingContentTemplate

-- | The scopes for any identifier can be represented as a stack. The most
-- immediate scope (e.g. local scope) is at the top of the stack, and the bottom
-- of the stack could contain global scope.
newtype Scopes = Scopes (Stack VariableMap)

--------------------------------------------------------------------------------

-- | Number of usages for each identifier.
type UsageMap = M.HashMap Identifier Int

--------------------------------------------------------------------------------
 
--------------------------------------------------------------------------------
