--------------------------------------------------------------------------------

module Decks.Parser where

import           Control.Monad                  ( liftM5 )
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
    | DecksDefStmt
        { defIdent           :: Identifier
        , defContentTemplate :: ContentTemplate
        }
    deriving (Eq, Show)

type ContentTemplate = Text

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent   :: Identifier
    , elAttrs   :: [DecksAttr]
    , elContent :: Maybe Content
    }
    deriving (Eq, Show)

type Content = Text

-- | Elements can have attributes attached to them, referring to external CSS
-- code.
data DecksAttr
    = CssId Text              -- ^ E.g. @#identifier@
    | CssClass Text           -- ^ E.g. @.class-name@
    | CssProp                 -- ^ E.g. @key="val"@
        { cssPropKey :: Text
        , cssPropVal :: Text
        }
    deriving (Eq, Show)

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
pStmt = pDrawStmt <|> pLetStmt <|> pDefStmt

pDrawStmt :: Parser DecksStmt
pDrawStmt = DecksDrawStmt <$> pElement

pLetStmt :: Parser DecksStmt
pLetStmt =
    DecksLetStmt
        <$> (string "!let" *> space1 *> pIdentifier)
        <*> (space *> char '=' *> space *> pElement)

pDefStmt :: Parser DecksStmt
pDefStmt =
    DecksDefStmt
        <$> (string "!def" *> space1 *> pIdentifier)
        <*> (space *> char '=' *> space *> braced pContentTemplate)

pElement :: Parser DecksElement
pElement =
    DecksElement
        <$> (pIdentifier <* space)
        <*> (fromMaybe [] <$> optional (bracketed (some pAttr) <* space))
        <*> optional (braced pContent)

pAttr :: Parser DecksAttr
pAttr = choice [pCssId, pCssClass, pCssProp]
  where
    pCssId    = CssId <$> (char '#' *> identChars)
    pCssClass = CssClass <$> (char '.' *> identChars)
    pCssProp  = CssProp <$> (identChars <* char '=') <*> optQuoted valueChars
        where valueChars = fmap T.pack . some . noneOf $ ['{', '}', '"']

-- TODO: Support more characters, and escaped characters (like braces)
pContent :: Parser Content
pContent = T.strip . T.pack <$> some allowedChars
    where allowedChars = noneOf ['{', '}']

pContentTemplate :: Parser Text
pContentTemplate = liftM5
    (\x1 x2 x3 x4 x5 -> T.concat [x1, x2, x3, x4, x5])
    allowedChars
    (templateStr "style")
    allowedChars
    (templateStr "content")
    allowedChars
  where
    allowedChars :: Parser Text
    allowedChars = fmap T.pack <$> some $ noneOf ['{', '}', '$']

    templateStr :: Text -> Parser Text
    templateStr name = char '$' *> string name <* char '$'

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

-- Optionally surrounded by double-quotation marks.
optQuoted :: Parser a -> Parser a
optQuoted f = (quoteChar *> f <* quoteChar) <|> f where quoteChar = char '"'

--------------------------------------------------------------------------------
