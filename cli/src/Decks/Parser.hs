--------------------------------------------------------------------------------

{-# LANGUAGE TypeApplications #-}

-- | Produces a parse tree from the input file.
--
module Decks.Parser where

import           Decks.AstShow
import           Decks.Grammar
import           Decks.Logging
import           Decks.Utils

import           Control.Monad

import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Void                      ( Void )

import           Data.Char                      ( isSpace )
import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

parseDecks :: FilePath -> IO (Maybe DecksProgram)
parseDecks path = do
    contents <- T.pack <$> readFile path
    case runParser pProgram path contents of
        Left bundle -> do
            logMsg LogError "Unable to parse file"
            putStrLn (errorBundlePretty bundle)
            pure Nothing
        Right ast -> do
            logMsg LogSuccess "Parsed successfully"
            TIO.putStrLn . T.intercalate "\n" . astShow "  " $ ast
            pure $ Just ast

pProgram :: Parser DecksProgram
pProgram = fmap DecksProgram $ many (pStmt <* many newline) <* eof

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
        <*> (fromMaybe [] <$> optional attrSection <* space)
        <*> optional (braced pContent)
  where
    attrSection = char '[' *> space *> attrList <* space <* char ']' <* space
    -- Space following an attribute needs lookahead and backtracking with 'try'
    -- since FOLLOW could be either ']' or another attribute.
    attrList    = liftM2 (:) pAttr (many $ try (space1 *> pAttr))

pAttr :: Parser DecksAttr
pAttr = choice [pCssId, pCssClass, pCssProp]
  where
    pCssId    = CssId <$> (char '#' *> identChars)
    pCssClass = CssClass <$> (char '.' *> identChars)
    pCssProp  = CssProp <$> (identChars <* char '=') <*> optQuoted valueChars
      where
        valueChars = fmap T.pack . some $ satisfy tokPred
        tokPred = liftM2 (&&) (`notElem` ("{}\"[]" :: String)) (not . isSpace)

-- TODO: Support more characters, and escaped characters (like braces)
pContent :: Parser Content
pContent = Content . T.strip . T.pack <$> some allowedChars
    where allowedChars = noneOf ['{', '}']

pContentTemplate :: Parser ContentTemplate
pContentTemplate =
    ContentTemplate
        .   T.concat
        <$> liftM5 list5
                   allowedChars
                   (templateStr "style")
                   allowedChars
                   (templateStr "content")
                   allowedChars
  where
    allowedChars :: Parser Text
    allowedChars = fmap T.pack <$> some $ noneOf ['{', '}', '$']

    templateStr :: Text -> Parser Text
    templateStr name = T.concat <$> liftM3 list3
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
optQuoted f = f <|> (quoteChar *> f <* quoteChar) where quoteChar = char '"'

--------------------------------------------------------------------------------
