--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types representing parts of a Decks DSL document, independent of how it is
-- parsed and generated.
--
module Decks.Document where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec

--------------------------------------------------------------------------------

newtype DecksDocument = DecksDocument FilePath
    deriving (Eq, Show)

newtype HtmlOutput = HtmlOutput FilePath
    deriving (Eq, Show)

instance FromJSON Pos
instance ToJSON Pos

instance FromJSON SourcePos where
    parseJSON = withObject "SourcePos" $ \v ->
        SourcePos <$> (v .: "path") <*> (v .: "line") <*> (v .: "col")

instance ToJSON SourcePos where
    toJSON (SourcePos path line col) =
        object ["path" .= path, "line" .= line, "col" .= col]

-- | Start and end positions in a document.
data SrcRange = SrcRange
    { rangeStart :: SourcePos
    , rangeEnd   :: SourcePos
    }
    deriving (Eq, Generic, Show)

instance FromJSON SrcRange
instance ToJSON SrcRange

--------------------------------------------------------------------------------
