--------------------------------------------------------------------------------

-- | Functions for adding special attributes Decks nodes, to add information.
--
module Decks.Compiler.CodeGen.Tagging where

import           Decks.Compiler.CodeGen.Attributes
import           Decks.Compiler.Grammar

--------------------------------------------------------------------------------

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
