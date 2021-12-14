--------------------------------------------------------------------------------

module Main where

import           Decks.Grammar
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
                                           , elContent = Just $ Content "bar"
                                           }
        it "has optional attrs" $ do
            parse pElement "" "foo { bar }" `shouldParse` DecksElement
                { elIdent   = Identifier "foo"
                , elAttrs   = []
                , elContent = Just $ Content "bar"
                }
        it "has optional attrs and content" $ do
            parse pElement "" "foo"
                `shouldParse` DecksElement (Identifier "foo") [] Nothing
        it "can parse multiple attributes" $ do
            parse pElement "" "foo [ .class #id x=0 y-prop=\"test\" ]"
                `shouldParse` DecksElement
                                  (Identifier "foo")
                                  [ CssClass "class"
                                  , CssId "id"
                                  , CssProp "x"      "0"
                                  , CssProp "y-prop" "test"
                                  ]
                                  Nothing

    describe "pAttr" $ do
        it "can recognise id selectors" $ do
            parse pAttr "" "#my-class" `shouldParse` CssId "my-class"
        it "can recognise class selectors" $ do
            parse pAttr "" ".my-class" `shouldParse` CssClass "my-class"
        it "can recognise key-value properties" $ do
            parse pAttr "" "height=25px" `shouldParse` CssProp "height" "25px"
        it "can recognise key-value properties in quotes" $ do
            parse pAttr "" "color=\"#FF0\"" `shouldParse` CssProp "color" "#FF0"

    describe "pContent" $ do
        it "can contain spaces, dashes, underscores" $ do
            let str = "Foo bar_bar - foo"
            parse pContent "" str `shouldParse` Content str
        it "can contain punctuation" $ do
            let str = "It's good? (yes - it is!)"
            parse pContent "" str `shouldParse` Content str
        it "can contain numeric equations" $ do
            let str = "2 + 2 = 5"
            parse pContent "" str `shouldParse` Content str
        it "trimmed spaces (no leading or trailing spaces)" $ do
            let str = "  foo bar  "
            parse pContent "" str `shouldParse` Content "foo bar"

    describe "pContentTemplate" $ do
        it "can parse a basic content template" $ do
            let ct = "<h1 $style$>$content$</h1>"
            parse pContentTemplate "" ct `shouldParse` ContentTemplate ct
        it "requires a style template string" $ do
            parse pContentTemplate ""
                `shouldFailOn` "<h1 $not a style$>$content$</h1>"
        it "requires a content template string" $ do
            parse pContentTemplate ""
                `shouldFailOn` "<h1 $style$>$not a content$</h1>"

    describe "pDrawStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pDrawStmt "" "bar [ .my-class]{ content }"
                `shouldParse` DecksDrawStmt
                                  (DecksElement
                                      { elIdent   = Identifier "bar"
                                      , elAttrs   = [CssClass "my-class"]
                                      , elContent = Just $ Content "content"
                                      }
                                  )

    describe "pLetStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pLetStmt "" "!let foo  =bar [ .my-class  ]    { content }"
                `shouldParse` DecksLetStmt
                                  (Identifier "foo")
                                  (DecksElement
                                      { elIdent   = Identifier "bar"
                                      , elAttrs   = [CssClass "my-class"]
                                      , elContent = Just $ Content "content"
                                      }
                                  )

    describe "pDefineStmt" $ do
        it "recognises a simple definition" $ do
            let ct = "<h1 $style$>$content$</h1>"
            parse pDefStmt "" ("!def h1 = {" <> ct <> "}")
                `shouldParse` DecksDefStmt
                                  (Identifier "h1")
                                  (ContentTemplate ct)


    describe "pStmt" $ do
        it "can recognise draw stmts" $ do
            parse pStmt "" "bar { overridden content }"
                `shouldParse` DecksDrawStmt
                                  (DecksElement
                                      (Identifier "bar")
                                      []
                                      (Just $ Content "overridden content")
                                  )
        it "can recognise let stmts" $ do
            parse pStmt "" "!let foo=bar [.my-class]" `shouldParse` DecksLetStmt
                (Identifier "foo")
                DecksElement { elIdent   = Identifier "bar"
                             , elAttrs   = [CssClass "my-class"]
                             , elContent = Nothing
                             }

--------------------------------------------------------------------------------
