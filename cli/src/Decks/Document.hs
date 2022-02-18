--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Types representing parts of a Decks DSL document, independent of how it is
-- parsed and generated.
--
module Decks.Document where

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Extra               as T
import qualified Data.Text.IO                  as TIO
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec

--------------------------------------------------------------------------------
-- Types of document

newtype DecksDocument = DecksDocument { unDecksDoc :: FilePath }
    deriving (Eq, Show)

newtype HtmlOutput = HtmlOutput FilePath
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Types of properties

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
-- Actions that can be performed on a document

-- | Applies 'editTextRange' to the file given by the file path.
docEditTextRange :: FilePath -> SrcRange -> Text -> IO ()
docEditTextRange path range newVal = do
    content <- TIO.readFile path
    print content
    let newContent = editTextRange content range newVal
    print newContent
    TIO.writeFile path newContent

--------------------------------------------------------------------------------
-- Helper functions

-- | 'editTextRange' @input range newVal@ replaces the text within the range of
-- the given input to @newVal@.
editTextRange :: Text -> SrcRange -> Text -> Text
editTextRange input (SrcRange start end) newVal =
    T.unlinesNoTrail before <> newVal <> T.unlines after
  where
    startl = unPos (sourceLine start)
    startc = unPos (sourceColumn start)
    endl   = unPos (sourceLine end)
    endc   = unPos (sourceColumn end)

    ls     = T.lines input

    before = take (startl - 1) ls ++ [T.take (startc - 1) (ls !! (startl - 1))]
    after  = T.drop (endc - 1) (ls !! (endl - 1)) : drop endl ls

--------------------------------------------------------------------------------
