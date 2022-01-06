--------------------------------------------------------------------------------

-- | Helper functions for generating Decks props as HTML attributes.
--
module Decks.Compiler.CodeGen.Attributes where


import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

-- | 'mkAttr' @name@ @mval@ returns the HTML code for an attribute with the
-- given name and possible value.
--
-- ==== __Examples__
--
-- >>> mkAttr "attribute-a" "x y z"
-- "attribute-a=\"x y z\""
--
-- >>> mkAttr "attribute-b" Nothing
-- "attribute-b"
--
mkAttr :: Text -> Maybe Text -> Text
mkAttr name mval = mkKeyValTxt "=" (name, quoteValue <$> mval)
    where quoteValue x = "\"" <> x <> "\""

-- | 'mkKeyValTxt' @delimiter@ @pair@ produces a string representing the pair.
mkKeyValTxt :: Text -> (Text, Maybe Text) -> Text
mkKeyValTxt _ (k, Nothing) = k
mkKeyValTxt d (k, Just v ) = k <> d <> v

--------------------------------------------------------------------------------
