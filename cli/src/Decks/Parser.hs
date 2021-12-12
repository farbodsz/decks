--------------------------------------------------------------------------------

module Decks.Parser where

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------------------------------------------------------

type DecksProgram = [DecksStmt]

-- | Identifies a drawable element.
newtype Identifier = Identifier Text
    deriving (Eq, Show)

data DecksStmt
    = DecksDrawStmt
        { drawElem :: DecksElement
        }
    | DecksLetStmt
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
-- TODO: CssStyle       -- ^ E.g. @key="val"@
--
data DecksAttr
    = CssId Text        -- ^ E.g. @#identifier@
    | CssClass Text     -- ^ E.g. @.class-name@
    deriving (Eq, Show)

type Content = Text

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

parseDecks :: FilePath -> IO ()
parseDecks path = do
    contents <- T.pack <$> readFile path
    putStrLn $ case runParser pProgram path contents of
        Left  bundle -> errorBundlePretty bundle
        Right ast    -> show ast

pProgram :: Parser DecksProgram
pProgram = many (pStmt <* many newline) <* eof

pStmt :: Parser DecksStmt
pStmt = pDrawStmt <|> pLetStmt

pDrawStmt :: Parser DecksStmt
pDrawStmt = DecksDrawStmt <$> pElement

pLetStmt :: Parser DecksStmt
pLetStmt =
    DecksLetStmt
        <$> (string "!let" *> space1 *> pIdentifier)
        <*> (space *> char '=' *> space *> pElement)

pElement :: Parser DecksElement
pElement =
    DecksElement
        <$> (pIdentifier <* space)
        <*> (fromMaybe [] <$> optional (bracketed (some pAttr) <* space))
        <*> optional (braced pContent)

pAttr :: Parser DecksAttr
pAttr = pCssId <|> pCssClass
  where
    pCssId    = CssId <$> (char '#' *> identChars)
    pCssClass = CssClass <$> (char '.' *> identChars)

-- TODO: Support more characters, and escaped characters (like braces)
pContent :: Parser Content
pContent = T.strip . T.pack <$> some allowedChars
    where allowedChars = noneOf ['{', '}']

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> identChars

-- | Text for an identifier starting with a letter and containing only
-- alphanumeric characters, dashes and underscores.
identChars :: Parser Text
identChars = T.pack <$> ((:) <$> letterChar <*> many alphaNumDashChar)
    where alphaNumDashChar = alphaNumChar <|> char '_' <|> char '-'

--------------------------------------------------------------------------------

-- Surrounded by braces and space(s).
braced :: Parser a -> Parser a
braced f = char '{' *> space *> f <* space <* char '}'

-- Surrounded by square brackets and space(s).
bracketed :: Parser a -> Parser a
bracketed f = char '[' *> space *> f <* space <* char ']'

--------------------------------------------------------------------------------
