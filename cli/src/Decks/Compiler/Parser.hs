--------------------------------------------------------------------------------

-- | Produces a parse tree from the input file.
--
module Decks.Compiler.Parser where

import           Decks.Compiler.AstShow
import           Decks.Compiler.Grammar
import           Decks.Logging
import           Decks.Utils

import           Control.Monad

import           Data.Char                      ( isSpace )
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )

import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

-- TODO: avoid IO here - return Either type so we can compose easily with
-- runCodeGen?
parseDecks :: FilePath -> Bool -> IO (Maybe DecksProgram)
parseDecks path verbose = do
    contents <- T.pack <$> readFile path
    case runParser pProgram path contents of
        Left bundle -> do
            logMsg LogError "Unable to parse file"
            putStrLn (errorBundlePretty bundle)
            pure Nothing
        Right ast -> do
            logMsg LogSuccess "Parsed successfully"
            when verbose
                $ TIO.putStrLn
                . T.intercalate "\n"
                . astShow "  "
                $ ast
            pure $ Just ast

pProgram :: Parser DecksProgram
pProgram = DecksProgram <$> some pStmt <* eof

pStmt :: Parser DecksStmt
pStmt = choice [pDrawStmt, pLetStmt, pDefStmt, pString, pComment]
    <* many (newline *> space)

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

pString :: Parser DecksStmt
pString = do
    start <- getSourcePos
    let singleLine = "\"" *> many (noneOf ['"', '\n', '\r']) <* "\""
        multiLine  = "[[" *> many (noneOf ['[', ']']) <* "]]"
    litContents <- T.strip . T.pack <$> (singleLine <|> multiLine)
    end         <- getSourcePos
    pure $ DecksString (SrcRange start end) litContents

pComment :: Parser DecksStmt
pComment = DecksComment <$> (string "//" *> commentChars)
    where commentChars = fmap T.pack . many $ noneOf ['\n', '\r']

pElement :: Parser DecksElement
pElement =
    DecksElement
        <$> (pIdentifier <* space)
        <*> (fromMaybe mempty <$> optional (bracketed pProps <* space) <* space)
        <*> (fromMaybe [] <$> optional (braced (some pStmt)))

pProps :: Parser DecksElemProps
pProps =
    DecksElemProps
        <$> (optional pId <* space)
        <*> many (pClass <* space)
        <*> many (pStyle <* space)
        <*> many (pAttr <* space)
  where
    pId    = char '#' *> identChars
    pClass = char '.' *> identChars
    pStyle =
        char '%' *> liftM2 (,) identChars (char '=' *> optQuoted valueChars)
    pAttr = (,) <$> identChars <*> optional (char '=' *> optQuoted valueChars)

    valueChars = fmap T.pack . some $ satisfy tokPred
      where
        tokPred = liftM2 (&&) (`notElem` ("{}\"[]" :: String)) (not . isSpace)

pContentTemplate :: Parser ContentTemplate
pContentTemplate =
    ContentTemplate
        .   T.concat
        <$> liftM5 mkList5
                   allowedChars
                   (templateStr "props")
                   allowedChars
                   (templateStr "content")
                   allowedChars
  where
    allowedChars :: Parser Text
    allowedChars = fmap T.pack <$> some $ noneOf ['{', '}', '$']

    templateStr :: Text -> Parser Text
    templateStr name = T.concat <$> liftM3 mkList3
                                           (T.singleton <$> char '$')
                                           (string name)
                                           (T.singleton <$> char '$')

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> identChars

-- | Text for an identifier starting with a letter and containing only
-- alphanumeric characters, dashes and underscores.
identChars :: Parser Text
identChars = T.pack <$> ((:) <$> letterChar <*> many alphaNumDashChar)
    where alphaNumDashChar = alphaNumChar <|> char '_' <|> char '-'

--------------------------------------------------------------------------------

-- | Surrounded by braces and space(s).
braced :: Parser a -> Parser a
braced f = char '{' *> space *> f <* space <* char '}'

-- | Surrounded by square brackets and space(s).
bracketed :: Parser a -> Parser a
bracketed f = char '[' *> space *> f <* space <* char ']'

-- | Optionally surrounded by double-quotation marks.
optQuoted :: Parser a -> Parser a
optQuoted f = f <|> quoteChar *> f <* quoteChar where quoteChar = char '"'

--------------------------------------------------------------------------------
