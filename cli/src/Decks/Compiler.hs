--------------------------------------------------------------------------------

-- | Combines the parsing and code generation stages.
--
module Decks.Compiler where

import           Decks.Compiler.CodeGen         ( runCodeGen )
import           Decks.Compiler.Parser          ( parseDecks )
import           Decks.Document                 ( DecksDocument
                                                , HtmlOutput
                                                )

--------------------------------------------------------------------------------

-- TODO: not in IO monad?
compile :: HtmlOutput -> Bool -> DecksDocument -> IO ()
compile outPath verbose file = parseDecks file verbose >>= \case
    Nothing  -> pure ()
    Just ast -> runCodeGen outPath verbose ast

--------------------------------------------------------------------------------
