module Main where

import           Decks.App
import           Decks.Parser

main :: IO ()
main = do
    cmd <- parseCmd
    parseDecks (optFilepath cmd)
