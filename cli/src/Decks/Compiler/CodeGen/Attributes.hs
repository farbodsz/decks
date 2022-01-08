--------------------------------------------------------------------------------

-- | Helper functions for generating Decks props as HTML attributes.
--
module Decks.Compiler.CodeGen.Attributes where

import           Decks.Utils                    ( fromList )

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

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

-- | Generates the value for the class string.
--
-- >>> genElemClassesVal ["first-class", "second-class"]
-- ".first-class .second-class"
--
genElemClassesVal :: [Text] -> Text
genElemClassesVal = T.unwords . map ("." <>)

-- | Generates the value for the style string.
--
-- >>> genElemStylesVal [("color", "blue"), ("display", "inline")]
-- "color:blue; display:inline;"
--
genElemStylesVal :: [(Text, Text)] -> Text
genElemStylesVal = fromList
    ""
    (T.unwords . map (\(k, v) -> mkKeyValTxt ":" (k, Just v) <> ";"))

--------------------------------------------------------------------------------
