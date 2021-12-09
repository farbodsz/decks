module Decks.Parser where

parseFile :: FilePath -> IO ()
parseFile path = do
    contents <- readFile path
    putStr contents
