--------------------------------------------------------------------------------

module Main where

import           Decks.Parser

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "pIdentifier" $ do
        it "can parse simple identifiers" $ do
            parse pIdentifier "" "fooBar" `shouldParse` Identifier "fooBar"

--------------------------------------------------------------------------------
