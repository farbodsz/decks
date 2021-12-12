--------------------------------------------------------------------------------

module Decks.Parser where

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------------------------------------------------------

type DecksProgram = [DecksLetStmt]

-- | Identifies a drawable element.
newtype Identifier = Identifier Text
    deriving (Eq, Show)

data DecksLetStmt = DecksLetStmt
    { letIdent :: Identifier
    , letElem  :: DecksElement
    }
    deriving (Eq, Show)

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent   :: Identifier
    , elAttrs   :: [DecksAttr]
    , elContent :: Maybe Content
    }
    deriving (Eq, Show)

-- | Elements can have CSS selectors attached to them.
--
-- TODO: CssIdentifier  -- ^ E.g. @#identifier@
-- TODO: CssStyle       -- ^ E.g. @key="val"@

data DecksAttr = CssClass Text -- ^ E.g. @.class-name@
    deriving (Eq, Show)

type Content = Text

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

parseDecks :: FilePath -> IO ()
parseDecks path = do
    contents <- T.pack <$> readFile path
    print $ runParser pProgram path contents

pProgram :: Parser DecksProgram
pProgram = many pLetStmt <* eof

pLetStmt :: Parser DecksLetStmt
pLetStmt =
    DecksLetStmt
        <$> (string "!let" *> space1 *> pIdentifier)
        <*> (space *> char '=' *> space *> pElement)

pElement :: Parser DecksElement
pElement =
    DecksElement
        <$> (pIdentifier <* space)
        <*> (fromMaybe [] <$> optional (pBracketed (some pAttr) <* space))
        <*> optional (pBraced pContent)

pAttr :: Parser DecksAttr
pAttr = pCssClass

pCssClass :: Parser DecksAttr
pCssClass = char '.' *> (CssClass <$> identChars)

-- TODO: Support more characters, and escaped characters (like braces)
pContent :: Parser Content
pContent = T.pack <$> some alphaNumChar

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> identChars

-- | Text for an identifier starting with a letter and containing only
-- alphanumeric characters, dashes and underscores.
identChars :: Parser Text
identChars = T.pack <$> ((:) <$> letterChar <*> many alphaNumDashChar)
    where alphaNumDashChar = alphaNumChar <|> char '_' <|> char '-'

--------------------------------------------------------------------------------

pBraced :: Parser a -> Parser a
pBraced f = char '{' *> space *> f <* space <* char '}'

pBracketed :: Parser a -> Parser a
pBracketed f = char '[' *> space *> f <* space <* char ']'

pAroundWs :: Parser a -> Parser a
pAroundWs f = space *> f <* space

--------------------------------------------------------------------------------
