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

-- | Runs a computation @f@ transforming a 'Text' input to 'Text' output, to the
-- document with the given 'FilePath'.
withDocument :: FilePath -> (Text -> Text) -> IO ()
withDocument path f = do
    contents <- TIO.readFile path
    let newContents = f contents
    TIO.writeFile path newContents

-- | Applies 'editTextRange' to the file given by the file path.
docEditTextRange :: FilePath -> SrcRange -> Text -> IO ()
docEditTextRange path range txt = withDocument path (editTextRange range txt)

-- | Inserts text at the end of the given source range.
docInsertText :: FilePath -> SrcRange -> Text -> IO ()
docInsertText path SrcRange {..} txt = withDocument
    path
    (insertTextAt insertPos insertStr)
  where
    -- Insert just before the end of the parent
    insertPos = rangeEnd { sourceColumn = addPos (-1) (sourceColumn rangeEnd) }
    insertStr = "[[ " <> txt <> " ]]"
    addPos x p = mkPos (unPos p + x)

--------------------------------------------------------------------------------
-- Helper functions

-- | 'editTextRange' @range newVal input@ replaces the text within the range of
-- the given input to @newVal@.
editTextRange :: SrcRange -> Text -> Text -> Text
editTextRange (SrcRange start end) newVal input =
    T.unlinesNoTrail before <> newVal <> T.unlines after
  where
    startl = unPos (sourceLine start)
    startc = unPos (sourceColumn start)
    endl   = unPos (sourceLine end)
    endc   = unPos (sourceColumn end)

    ls     = T.lines input

    before = take (startl - 1) ls ++ [T.take (startc - 1) (ls !! (startl - 1))]
    after  = T.drop (endc - 1) (ls !! (endl - 1)) : drop endl ls

-- | 'insertTextAt' @input position text@ inserts some text at the specified
-- position of the input.
--
-- Note that text insertion is simply a special case of text edit, with the
-- range being edited just a single position.
insertTextAt :: SourcePos -> Text -> Text -> Text
insertTextAt pos = editTextRange (SrcRange pos pos)

--------------------------------------------------------------------------------
