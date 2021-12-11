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
    { letIdent    :: Identifier
    , letDrawStmt :: DecksDrawStmt
    }
    deriving Show

data DecksDrawStmt = DecksDrawStmt
    { drawIdent :: Identifier
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
pProgram = many pLetStmt

pLetStmt :: Parser DecksLetStmt
pLetStmt = do
    _     <- string "!let"
    ident <- pAroundWs pIdentifier
    _     <- char '='
    draw  <- pAroundWs pDrawStmt
    pure $ DecksLetStmt ident draw

pDrawStmt :: Parser DecksDrawStmt
pDrawStmt = DecksDrawStmt <$> pIdentifier

pIdentifier :: Parser Text
pIdentifier = T.pack <$> some alphaNumChar

pAroundWs :: Parser a -> Parser a
pAroundWs f = space *> f <* space

--------------------------------------------------------------------------------
