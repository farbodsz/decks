--------------------------------------------------------------------------------

-- | Functions for generating HTML from AST nodes.
--
module Decks.CodeGen.Generate where

import           Decks.CodeGen.Types
import           Decks.Error
import           Decks.Grammar

import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State

import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

-- | Returns either successfully generated HTML from the Decks AST, or an error.
genProgram :: DecksProgram -> DecksM Html
genProgram (DecksProgram stmts) = mapM genStmt stmts <&> T.concat

-- | Generates HTML output from a Decks statement.
genStmt :: DecksStmt -> DecksM Html
genStmt (DecksDrawStmt l  ) = withStateT (markUsage (elIdent l)) (genElement l)
genStmt (DecksDefStmt i ct) = do
    ident <- getUniqIdent i
    withStateT (insertDef ident ct) (pure mempty)
genStmt (DecksLetStmt i DecksElement {..}) = do
    ident <- getUniqIdent i
    pct   <- getIdentPct elIdent
    withStateT
        (markUsage elIdent . insertLet ident (updatePct pct elAttrs elContent))
        (pure mempty)
genStmt (DecksComment _) = pure mempty

genElement :: DecksElement -> DecksM Html
genElement DecksElement {..} = do
    pct <- getIdentPct elIdent
    lift $ fillContentTemplate $ updatePct pct elAttrs elContent

-- | Returns the HTML representing a filled-in content template.
fillContentTemplate :: PendingContentTemplate -> HtmlResult
fillContentTemplate (PendingContentTemplate (ContentTemplate ct) as mc) = do
    attrs   <- fillCtAttrs as
    content <- maybe (Left EmptyContent) (Right . unContent) mc
    pure . T.replace "$attrs$" attrs . T.replace "$content$" content $ ct

data HtmlAttributes = HtmlAttributes
    { attrIdent   :: HtmlResult
    , attrClasses :: HtmlResult
    , attrStyles  :: HtmlResult
    }

-- | Returns the HTML attributes text corresponding to the Decks attributes,
-- i.e. HTML @id@, @class@, and @style@ attributes.
fillCtAttrs :: [DecksAttr] -> HtmlResult
fillCtAttrs = attrsToHtml . processAttributes
  where
    attrsToHtml :: HtmlAttributes -> HtmlResult
    attrsToHtml HtmlAttributes {..} =
        fmap (T.strip . T.unwords)
            . sequenceA
            $ [attrIdent, attrClasses, attrStyles]

    processAttributes :: [DecksAttr] -> HtmlAttributes
    processAttributes as =
        let isId (CssId _) = True
            isId _         = False
            isClass (CssClass _) = True
            isClass _            = False
            isStyle (CssProp _ _) = True
            isStyle _             = False
            process converter predicate = converter . filter predicate $ as
        in  HtmlAttributes { attrIdent   = process idsToHtml isId
                           , attrClasses = process classesToHtml isClass
                           , attrStyles  = process stylesToHtml isStyle
                           }

    idsToHtml :: [DecksAttr] -> HtmlResult
    idsToHtml []        = Right ""
    idsToHtml [CssId i] = Right $ "id=\"" <> i <> "\""
    idsToHtml _         = Left MultipleCssIds

    classesToHtml :: [DecksAttr] -> HtmlResult
    classesToHtml [] = Right ""
    classesToHtml cs =
        let clsNames = T.unwords $ map (\(CssClass name) -> "." <> name) cs
        in  Right $ "class=\"" <> clsNames <> "\""

    stylesToHtml :: [DecksAttr] -> HtmlResult
    stylesToHtml [] = Right ""
    stylesToHtml ps =
        let kvTexts = T.unwords $ map (\(CssProp k v) -> k <> ":" <> v) ps
        in  Right $ "style=\"" <> kvTexts <> "\""

--------------------------------------------------------------------------------
