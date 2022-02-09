--------------------------------------------------------------------------------

-- | Stack data structure and functions.
--
module Data.Stack where

--------------------------------------------------------------------------------

newtype Stack a = Stack [a]
    deriving (Eq, Monoid, Semigroup, Show)

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack $ x : xs

peek :: Stack a -> Maybe a
peek (Stack []     ) = Nothing
peek (Stack (x : _)) = Just x

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack []      ) = Nothing
pop (Stack (x : xs)) = Just (Stack xs, x)

size :: Stack a -> Int
size (Stack xs) = length xs

isEmpty :: Stack a -> Bool
isEmpty (Stack xs) = null xs

--------------------------------------------------------------------------------
