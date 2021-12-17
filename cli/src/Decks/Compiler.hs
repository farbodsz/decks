--------------------------------------------------------------------------------

-- | Combines the parsing and code generation stages.
--
module Decks.Compiler where

import           Decks.Compiler.CodeGen         ( runCodeGen )
import           Decks.Compiler.Parser          ( parseDecks )

--------------------------------------------------------------------------------

compile :: FilePath -> Bool -> FilePath -> IO ()
compile outPath verbose file = parseDecks file verbose >>= \case
    Nothing  -> pure ()
    Just ast -> runCodeGen outPath verbose ast

--------------------------------------------------------------------------------
