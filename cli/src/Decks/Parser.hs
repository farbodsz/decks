--------------------------------------------------------------------------------

module Decks.Parser where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------------------------------------------------------

type DecksProgram = [DecksLetStmt]

type Identifier = Text

data DecksLetStmt = DecksLetStmt
    { letIdent :: Identifier
    , letElem  :: DecksElement
    }
    deriving Show

type Content = Text

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent   :: Identifier
    , elContent :: Content
    }
    deriving Show

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
pLetStmt = do
    _     <- string "!let"
    ident <- pAroundWs pIdentifier
    _     <- char '='
    el    <- pAroundWs pElement
    pure $ DecksLetStmt ident el

pElement :: Parser DecksElement
pElement = DecksElement <$> pAroundWs pIdentifier <*> pBraced pContent

-- TODO: Support more characters, and escaped characters (like braces)
pContent :: Parser Content
pContent = T.pack <$> some alphaNumChar

pIdentifier :: Parser Identifier
pIdentifier = T.pack <$> some alphaNumChar

--------------------------------------------------------------------------------

pBraced :: Parser a -> Parser a
pBraced f = char '{' *> space *> f <* space <* char '}'

pAroundWs :: Parser a -> Parser a
pAroundWs f = space *> f <* space

--------------------------------------------------------------------------------
