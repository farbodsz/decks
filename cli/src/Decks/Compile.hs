--------------------------------------------------------------------------------

-- | Combines the parsing and code generation stages.
--
module Decks.Compile where

import           Decks.CodeGen                  ( runCodeGen )
import           Decks.Parser                   ( parseDecks )

--------------------------------------------------------------------------------

compile :: FilePath -> Bool -> FilePath -> IO ()
compile outPath verbose file = parseDecks file verbose >>= \case
    Nothing  -> pure ()
    Just ast -> runCodeGen outPath verbose ast

--------------------------------------------------------------------------------
