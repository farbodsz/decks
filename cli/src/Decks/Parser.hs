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

-- | A drawable element statement.
data DecksElement = DecksElement
    { elIdent :: Identifier
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
    el    <- pAroundWs pElement
    pure $ DecksLetStmt ident el

pElement :: Parser DecksElement
pElement = DecksElement <$> pIdentifier

pIdentifier :: Parser Text
pIdentifier = T.pack <$> some alphaNumChar

pAroundWs :: Parser a -> Parser a
pAroundWs f = space *> f <* space

--------------------------------------------------------------------------------
