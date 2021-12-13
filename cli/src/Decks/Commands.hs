--------------------------------------------------------------------------------

-- | Responsible for definining and parsing various CLI options/arguments.
--
module Decks.Commands where

import           Options.Applicative

--------------------------------------------------------------------------------

data Opts = Opts
    { optFilepath :: FilePath
    , optWatch    :: Bool
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts =
    Opts
        <$> (argument
                str
                (  metavar "PATH"
                <> help "Directory path containing a Decks file to parse."
                )
            )
        <*> (not <$> switch
                (long "no-watch" <> short 'n' <> help
                    "Only run Decks once, not updating on subsequent changes"
                )
            )

--------------------------------------------------------------------------------
