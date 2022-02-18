--------------------------------------------------------------------------------

-- | Utility functions for working with 'Text'.
--
module Data.Text.Extra where

import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

-- | Like 'T.unlines' but removes the trailing newline from the last line.
unlinesNoTrail :: [Text] -> Text
unlinesNoTrail ts =
    let unlined = T.unlines ts
    in  fromMaybe unlined (T.stripSuffix "\n" unlined)

--------------------------------------------------------------------------------
