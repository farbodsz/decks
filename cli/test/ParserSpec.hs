module ParserSpec
    ( spec
    ) where

import           Decks.Compiler.Grammar
import           Decks.Compiler.Parser

import           Control.Monad                  ( forM_ )
import qualified Data.Text.IO                  as TIO
import           System.Directory               ( getDirectoryContents )
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "parser" $ testFiles >> testUnits

testFiles :: Spec
testFiles = describe "test files" $ do
    let resDir = "test/res/input/"
    fileNames <-
        runIO $ filter (`notElem` [".", ".."]) <$> getDirectoryContents resDir

    forM_ fileNames $ \file -> do
        let fpath = resDir ++ file
        it fpath $ do
            input <- TIO.readFile fpath
            parse pProgram "" `shouldSucceedOn` input

testUnits :: Spec
testUnits = describe "unit tests" $ do
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
            parse pElement "" "foo [.my-class] { \"bar\" }"
                `shouldParse` DecksElement
                                  { elIdent = Identifier "foo"
                                  , elProps = mempty
                                                  { propClasses = ["my-class"]
                                                  }
                                  , elStmts = [DecksLiteral "bar"]
                                  }
        it "has optional attrs" $ do
            parse pElement "" "foo { \"bar\" }" `shouldParse` DecksElement
                { elIdent = Identifier "foo"
                , elProps = mempty
                , elStmts = [DecksLiteral "bar"]
                }
        it "has optional props and content" $ do
            parse pElement "" "foo"
                `shouldParse` DecksElement (Identifier "foo") mempty []
        it "can parse multiple attributes" $ do
            parse pElement "" "foo [ #id .class %x=0 %y-prop=\"test\" z ]"
                `shouldParse` DecksElement
                                  (Identifier "foo")
                                  (DecksElemProps
                                      { propId      = Just "id"
                                      , propClasses = ["class"]
                                      , propStyles  = [ ("x"     , "0")
                                                      , ("y-prop", "test")
                                                      ]
                                      , propAttrs   = [("z", Nothing)]
                                      }
                                  )
                                  []
        it "can include let inside" $ do
            let
                str
                    = "foo {\n\
                \   !let bar = oof [.custom-class] { \"default\" }\n\
                \   bar\n\
                \   bar { \"overridden\" }\n\
                \ }\
                \"
            parse pElement "" str `shouldParse` DecksElement
                (Identifier "foo")
                mempty
                [ DecksLetStmt
                    { letIdent = Identifier "bar"
                    , letElem  = DecksElement
                                     { elIdent = Identifier "oof"
                                     , elProps = mempty
                                                     { propClasses =
                                                         ["custom-class"]
                                                     }
                                     , elStmts = [DecksLiteral "default"]
                                     }
                    }
                , DecksDrawStmt (DecksElement (Identifier "bar") mempty [])
                , DecksDrawStmt
                    (DecksElement (Identifier "bar")
                                  mempty
                                  [DecksLiteral "overridden"]
                    )
                ]

    describe "pLiteral" $ do
        it "can contain spaces, dashes, underscores" $ do
            let str = "\"Foo-bar_foo\""
            parse pLiteral "" str `shouldParse` DecksLiteral "Foo-bar_foo"
        it "can contain punctuation" $ do
            let str = "\"It's good? (yes - it is!)\""
            parse pLiteral "" str
                `shouldParse` DecksLiteral "It's good? (yes - it is!)"
        it "supports multi line syntax" $ do
            let
                str
                    = "[[\n\
                \ Here is a\n\
                \ multiline string\n\
                \ ]]"
            parse pLiteral "" str
                `shouldParse` DecksLiteral "Here is a\n multiline string"
        it "multi line with regular syntax fails" $ do
            let str = "\"Multi \n\
                \ line\""
            parse pLiteral "" `shouldFailOn` str
        it "can contain numeric equations" $ do
            let str = "[[2 + 2 = 5]]"
            parse pLiteral "" str `shouldParse` DecksLiteral "2 + 2 = 5"
        it "trimmed spaces (no leading or trailing spaces)" $ do
            let str = "\"  foo bar\"  "
            parse pLiteral "" str `shouldParse` DecksLiteral "foo bar"

    describe "pProps" $ do
        it "can recognise HTML id property" $ do
            parse pProps "" "#my-ident"
                `shouldParse` mempty { propId = Just "my-ident" }
        it "can recognise class property" $ do
            parse pProps "" ".my-class"
                `shouldParse` mempty { propClasses = ["my-class"] }
        it "can recognise style property" $ do
            parse pProps "" "%height=25px"
                `shouldParse` mempty { propStyles = [("height", "25px")] }
        it "can recognise style with values in quotes" $ do
            parse pProps "" "%color=\"#FF0\""
                `shouldParse` mempty { propStyles = [("color", "#FF0")] }
        it "can recognise a standalone HTML attribute" $ do
            parse pProps "" "data-template"
                `shouldParse` mempty
                                  { propAttrs = [("data-template", Nothing)]
                                  }
        it "can recognise a HTML attribute with value" $ do
            parse pProps "" "data-custom=5"
                `shouldParse` mempty { propAttrs = [("data-custom", Just "5")] }

    describe "pContentTemplate" $ do
        it "can parse a basic content template" $ do
            let ct = "<h1 $props$>$content$</h1>"
            parse pContentTemplate "" ct `shouldParse` ContentTemplate ct
        it "requires an attribute template string" $ do
            parse pContentTemplate ""
                `shouldFailOn` "<h1 $not an attr$>$content$</h1>"
        it "requires a content template string" $ do
            parse pContentTemplate ""
                `shouldFailOn` "<h1 $props$>$not a content$</h1>"

    describe "pDrawStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pDrawStmt "" "bar [ .my-class]{ \"content\" }"
                `shouldParse` DecksDrawStmt
                                  (DecksElement
                                      { elIdent = Identifier "bar"
                                      , elProps = mempty
                                                      { propClasses =
                                                          ["my-class"]
                                                      }
                                      , elStmts = [DecksLiteral "content"]
                                      }
                                  )

    describe "pLetStmt" $ do
        it "ignores extra whitespaces" $ do
            parse pLetStmt "" "!let foo  =bar [ .my-class  ]   { \"content\"}"
                `shouldParse` DecksLetStmt
                                  (Identifier "foo")
                                  (DecksElement
                                      { elIdent = Identifier "bar"
                                      , elProps = mempty
                                                      { propClasses =
                                                          ["my-class"]
                                                      }
                                      , elStmts = [DecksLiteral "content"]
                                      }
                                  )

    describe "pDefineStmt" $ do
        it "recognises a simple definition" $ do
            let ct = "<h1 $props$>$content$</h1>"
            parse pDefStmt "" ("!def h1 = {" <> ct <> "}")
                `shouldParse` DecksDefStmt
                                  (Identifier "h1")
                                  (ContentTemplate ct)


    describe "pStmt" $ do
        it "can recognise draw stmts" $ do
            parse pStmt "" "bar { [[overridden content]] }"
                `shouldParse` DecksDrawStmt
                                  (DecksElement
                                      (Identifier "bar")
                                      mempty
                                      [DecksLiteral "overridden content"]
                                  )
        it "can recognise let stmts" $ do
            parse pStmt "" "!let foo=bar [.my-class]" `shouldParse` DecksLetStmt
                (Identifier "foo")
                DecksElement { elIdent = Identifier "bar"
                             , elProps = mempty { propClasses = ["my-class"] }
                             , elStmts = []
                             }

--------------------------------------------------------------------------------
