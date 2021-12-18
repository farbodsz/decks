--------------------------------------------------------------------------------

-- | Utility functions.
--
module Decks.Utils where

import Data.Text (Text)

--------------------------------------------------------------------------------

type Html = Text

type URL = Text

--------------------------------------------------------------------------------

-- | A list of the given three arguments.
list3 :: a -> a -> a -> [a]
list3 x1 x2 x3 = [x1, x2, x3]

-- | A list of the given five arguments.
list5 :: a -> a -> a -> a -> a -> [a]
list5 x1 x2 x3 x4 x5 = [x1, x2, x3, x4, x5]

--------------------------------------------------------------------------------
