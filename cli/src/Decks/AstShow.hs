--------------------------------------------------------------------------------

-- | Pretty printing of the Decks AST.
--
module Decks.AstShow where

import           Decks.Grammar

import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

class AstShow a where
    -- | 'astShow' @indentChars node@ returns a list of lines representing the
    -- AST node.
    astShow :: Text -> a -> [Text]


instance AstShow DecksProgram where
    astShow w (DecksProgram stmts) =
        treeFmt w "Program" $ concatMap (astShow w) stmts

instance AstShow Identifier where
    astShow _ (Identifier i) = ["Identifier " <> i]

instance AstShow DecksStmt where
    astShow w (DecksDrawStmt el) = treeFmt w "DrawStmt" $ astShow w el
    astShow w (DecksLetStmt i el) =
        treeFmt w "LetStmt" $ astShow w i ++ astShow w el
    astShow w (DecksDefStmt i ct) =
        treeFmt w "DefStmt" $ astShow w i ++ astShow w ct
    astShow w (DecksLiteral txt) = treeFmt w "Literal" [txt]
    astShow w (DecksComment txt) = treeFmt w "Comment" [txt]

instance AstShow DecksElement where
    astShow w (DecksElement i as stmts) = treeFmt w "Element"
        $ concat [identTxt, attrsTxt, contTxt]
      where
        identTxt = astShow w i
        attrsTxt = concatMap (astShow w) as
        contTxt  = concatMap (astShow w) stmts

instance AstShow DecksAttr where
    astShow _ (CssId    i ) = ["CssId " <> i]
    astShow _ (CssClass c ) = ["CssClass " <> c]
    astShow _ (CssProp k v) = ["CssProp " <> k <> " = " <> v]

instance AstShow ContentTemplate where
    astShow w (ContentTemplate ct) = treeFmt w "ContentTemplate " [ct]

--------------------------------------------------------------------------------

-- | 'treeFmt' produces a list of Text lines from an AST node's text and the
-- texts of its children.
treeFmt :: Text -> Text -> [Text] -> [Text]
treeFmt indent root children = root : ((indent <>) <$> children)

--------------------------------------------------------------------------------
