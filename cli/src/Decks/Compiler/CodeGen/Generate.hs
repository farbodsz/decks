--------------------------------------------------------------------------------

-- | Functions for generating HTML from AST nodes.
--
module Decks.Compiler.CodeGen.Generate where

import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.State
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Decks.Compiler.CodeGen.Attributes
import           Decks.Compiler.CodeGen.Tagging
import           Decks.Compiler.CodeGen.Types
import           Decks.Compiler.Grammar
import           Decks.Utils

--------------------------------------------------------------------------------

-- | Returns either successfully generated HTML from the Decks AST, or an error.
genProgram :: DecksProgram -> DecksM Html
genProgram (DecksProgram stmts) = genStmts stmts <&> (<> "\n")

genStmts :: [DecksStmt] -> DecksM Html
genStmts stmts = mapM genStmt stmts <&> T.concat

-- | Generates HTML output from a Decks statement.
genStmt :: DecksStmt -> DecksM Html
genStmt (DecksDrawStmt src el) = withStateT (markUsage (elIdent el))
                                            (genElement taggedEl)
    where taggedEl = modifyPropsWith (tagElemRange src) el
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
genStmt (DecksString src txt) = genElement
    $ DecksElement literalTemplateIdentifier elemProps elemStmts
  where
    elemProps = tagElemRange src mempty
    elemStmts = [DecksLiteral $ T.replace "\n" "<br>" txt]
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
    pure
        . T.strip
        . T.replace " $props$" (T.stripEnd $ " " <> props)
        . T.replace "$content$" ctnt
        $ ct

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
fillCtProps :: DecksElemProps -> HtmlResult
fillCtProps = propsAsHtml . processProps . addDecksAttrs
  where
    propsAsHtml :: HtmlAttributes -> HtmlResult
    propsAsHtml HtmlAttributes {..} =
        fmap (T.strip . T.unwords)
            . sequenceA
            $ [attrIdent, attrClasses, attrStyles, attrAttrs]

    processProps :: DecksElemProps -> HtmlAttributes
    processProps DecksElemProps {..} = HtmlAttributes
        { attrIdent   = idToHtml propsId
        , attrClasses = classesToHtml propsClasses
        , attrStyles  = stylesToHtml propsStyles
        , attrAttrs   = attrsToHtml propsAttrs
        }

    idToHtml :: Maybe Text -> HtmlResult
    idToHtml Nothing      = Right ""
    idToHtml mid@(Just _) = Right $ mkAttr "id" mid

    classesToHtml :: [Text] -> HtmlResult
    classesToHtml [] = Right ""
    classesToHtml cs = Right $ mkAttr "class" (Just $ genElemClassesVal cs)

    stylesToHtml :: [(Text, Text)] -> HtmlResult
    stylesToHtml [] = Right ""
    stylesToHtml ps = Right $ mkAttr "style" (Just $ genElemStylesVal ps)

    attrsToHtml :: [(Text, Maybe Text)] -> HtmlResult
    attrsToHtml = Right . T.unwords . map
        (\(k, mv) -> mkAttr k (quoted <$> mv))
        where quoted t = "\"" <> t <> "\""

    addDecksAttrs :: DecksElemProps -> DecksElemProps
    addDecksAttrs = tagElemClass . tagElemStyles

--------------------------------------------------------------------------------
