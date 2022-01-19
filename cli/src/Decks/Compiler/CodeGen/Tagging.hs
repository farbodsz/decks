--------------------------------------------------------------------------------

-- | Functions for adding special attributes Decks nodes, to add information.
--
module Decks.Compiler.CodeGen.Tagging where

import           Decks.Compiler.CodeGen.Attributes
import           Decks.Compiler.Grammar

import           Control.Monad                  ( liftM2 )
import qualified Data.Text                     as T
import           Text.Megaparsec.Pos

--------------------------------------------------------------------------------

-- | Adds @data-decks-start@ and @data-decks-end@ tags given a source range.
tagElemRange :: SrcRange -> DecksElemProps -> DecksElemProps
tagElemRange (SrcRange start end) currProps = currProps
    { propsAttrs = [ ("data-decks-start", mkPosStr start)
                   , ("data-decks-end"  , mkPosStr end)
                   ]
    }
  where
    mkPosStr = Just . T.pack . show . liftM2 (,)
                                             (unPos . sourceLine)
                                             (unPos . sourceColumn)

-- | Adds the @data-decks-class@ attribute to an element's props, using its
-- current HTML classes.
tagElemClass :: DecksElemProps -> DecksElemProps
tagElemClass ps = case propsClasses ps of
    [] -> ps
    cs -> addElemAttr ("data-decks-class", Just (genElemClassesVal cs)) ps

-- | Adds the @data-decks-style@ attribute to an element's props, using its
-- current HTML styles.
tagElemStyles :: DecksElemProps -> DecksElemProps
tagElemStyles ps = case propsStyles ps of
    [] -> ps
    ss -> addElemAttr ("data-decks-style", Just (genElemStylesVal ss)) ps

--------------------------------------------------------------------------------
