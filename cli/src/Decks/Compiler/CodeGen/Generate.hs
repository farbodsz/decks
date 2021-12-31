--------------------------------------------------------------------------------

-- | Functions for generating HTML from AST nodes.
--
module Decks.Compiler.CodeGen.Generate where

import           Decks.Compiler.CodeGen.Types
import           Decks.Compiler.Error
import           Decks.Compiler.Grammar
import           Decks.Utils                    ( Html )

import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State

import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

-- | Returns either successfully generated HTML from the Decks AST, or an error.
genProgram :: DecksProgram -> DecksM Html
genProgram (DecksProgram stmts) = genStmts stmts

genStmts :: [DecksStmt] -> DecksM Html
genStmts stmts = mapM genStmt stmts <&> T.concat

-- | Generates HTML output from a Decks statement.
genStmt :: DecksStmt -> DecksM Html
genStmt (DecksDrawStmt l  ) = withStateT (markUsage (elIdent l)) (genElement l)
genStmt (DecksDefStmt i ct) = do
    ident <- getUniqIdent i
    withStateT (insertDef ident ct) (pure mempty)
genStmt (DecksLetStmt i DecksElement {..}) = do
    ident   <- getUniqIdent i
    pct     <- getIdentPct elIdent
    content <- genStmts elStmts
    withStateT
        (markUsage elIdent . insertLet ident (updatePct pct elProps content))
        (pure mempty)
genStmt (DecksLiteral txt) = pure txt
genStmt (DecksComment _  ) = pure mempty

genElement :: DecksElement -> DecksM Html
genElement DecksElement {..} = do
    pct     <- getIdentPct elIdent
    content <- genStmts elStmts
    lift $ fillContentTemplate $ updatePct pct elProps content

-- | Returns the HTML representing a filled-in content template.
fillContentTemplate :: PendingContentTemplate -> HtmlResult
fillContentTemplate (PendingContentTemplate (ContentTemplate ct) ps ctnt) = do
    props <- fillCtProps ps
    pure . T.replace "$attrs$" props . T.replace "$content$" ctnt $ ct

-- | Attributes in HTML, corresponding to the properties set on the Decks
-- element.
data HtmlAttributes = HtmlAttributes
    { attrIdent   :: HtmlResult
    , attrClasses :: HtmlResult
    , attrStyles  :: HtmlResult
    , attrAttrs   :: HtmlResult
    }

-- | Returns the HTML attributes text corresponding to the properties on the
-- Decks element.
fillCtProps :: [DecksElemProp] -> HtmlResult
fillCtProps = attrsToHtml . processProps
  where
    attrsToHtml :: HtmlAttributes -> HtmlResult
    attrsToHtml HtmlAttributes {..} =
        fmap (T.strip . T.unwords)
            . sequenceA
            $ [attrIdent, attrClasses, attrStyles, attrAttrs]

    processProps :: [DecksElemProp] -> HtmlAttributes
    processProps ps =
        let isId (ElemPropId _) = True
            isId _              = False
            isClass (ElemPropClass _) = True
            isClass _                 = False
            isStyle (ElemPropStyle _ _) = True
            isStyle _                   = False
            isAttr (ElemPropAttr _) = True
            isAttr _                = False
            process converter predicate = converter . filter predicate $ ps
        in  HtmlAttributes
                { attrIdent   = process idsToHtml isId
                , attrClasses = process classesToHtml isClass
                , attrStyles  = process stylesToHtml isStyle
                , attrAttrs   = process (Right . T.unwords . map propAttrName)
                                        isAttr
                }

    idsToHtml :: [DecksElemProp] -> HtmlResult
    idsToHtml []             = Right ""
    idsToHtml [ElemPropId i] = Right $ "id=\"" <> i <> "\""
    idsToHtml _              = Left MultipleElemPropIds

    classesToHtml :: [DecksElemProp] -> HtmlResult
    classesToHtml [] = Right ""
    classesToHtml cs =
        let clsNames =
                T.unwords $ map (\(ElemPropClass name) -> "." <> name) cs
        in  Right $ "class=\"" <> clsNames <> "\""

    stylesToHtml :: [DecksElemProp] -> HtmlResult
    stylesToHtml [] = Right ""
    stylesToHtml ps =
        let kvTexts = T.unwords
                $ map (\(ElemPropStyle k v) -> T.concat [k, ":", v, ";"]) ps
        in  Right $ "style=\"" <> kvTexts <> "\""

--------------------------------------------------------------------------------
