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
        it "can contain dashes and underscores" $ do
            parse pIdentifier "" "foo_bar-Foo"
                `shouldParse` Identifier "foo_bar-Foo"
        it "should not contain braces" $ do
            parse pIdentifier "" "fo{o}Bar" `shouldParse` Identifier "fo"
        it "should not start with a number" $ do
            parse pIdentifier "" `shouldFailOn` "4ooBar"

    describe "pElement" $ do
        it "can parse simple element" $ do
            parse pElement "" "foo { bar }"
                `shouldParse` DecksElement (Identifier "foo") "bar"

    describe "pLetStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pLetStmt "" "!let foo  =bar { content }"
                `shouldParse` DecksLetStmt
                                  { letIdent = Identifier "foo"
                                  , letElem  = DecksElement
                                                   (Identifier "bar")
                                                   "content"
                                  }

--------------------------------------------------------------------------------
