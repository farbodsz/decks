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
            parse pElement "" "foo [.my-class] { bar }"
                `shouldParse` DecksElement { elIdent   = Identifier "foo"
                                           , elAttrs   = [CssClass "my-class"]
                                           , elContent = Just "bar"
                                           }
        it "has optional attrs" $ do
            parse pElement "" "foo { bar }" `shouldParse` DecksElement
                { elIdent   = Identifier "foo"
                , elAttrs   = []
                , elContent = Just "bar"
                }
        it "has optional attrs and content" $ do
            parse pElement "" "foo"
                `shouldParse` DecksElement (Identifier "foo") [] Nothing

    describe "pDrawStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pDrawStmt "" "bar [ .my-class]{ content }"
                `shouldParse` DecksDrawStmt
                                  { drawElem = DecksElement
                                                   { elIdent = Identifier "bar"
                                                   , elAttrs = [ CssClass
                                                                     "my-class"
                                                               ]
                                                   , elContent = Just "content"
                                                   }
                                  }


    describe "pLetStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pLetStmt "" "!let foo  =bar [ .my-class  ]    { content }"
                `shouldParse` DecksLetStmt
                                  { letIdent = Identifier "foo"
                                  , letElem  = DecksElement
                                                   { elIdent = Identifier "bar"
                                                   , elAttrs = [ CssClass
                                                                     "my-class"
                                                               ]
                                                   , elContent = Just "content"
                                                   }
                                  }

    describe "pStmt" $ do
        it "can recognise draw stmts" $ do
            parse pStmt "" "bar { overridden content }"
                `shouldParse` DecksDrawStmt
                                  { drawElem = DecksElement
                                                   (Identifier "bar")
                                                   []
                                                   (Just "overridden content")
                                  }

        it "can recognise let stmts" $ do
            parse pStmt "" "!let foo=bar [.my-class]" `shouldParse` DecksLetStmt
                (Identifier "foo")
                DecksElement { elIdent   = Identifier "bar"
                             , elAttrs   = [CssClass "my-class"]
                             , elContent = Nothing
                             }

--------------------------------------------------------------------------------
