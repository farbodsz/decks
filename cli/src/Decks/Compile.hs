--------------------------------------------------------------------------------

-- | Combines the parsing and code generation stages.
--
module Decks.Compile where

import           Decks.CodeGen                  ( runCodeGen )
import           Decks.Parser                   ( parseDecks )

--------------------------------------------------------------------------------

compile :: FilePath -> IO ()
compile file = parseDecks file >>= \case
    Nothing  -> pure ()
    Just ast -> runCodeGen ast

--------------------------------------------------------------------------------
