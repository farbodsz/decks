module Decks.App
    ( parseCmd
    , Opts(..)
    ) where

import           Options.Applicative

data Opts = Opts
    { optFilepath :: FilePath
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts = Opts <$> argument
    str
    (metavar "FILEPATH" <> help "Path for the Decks file to parse.")
