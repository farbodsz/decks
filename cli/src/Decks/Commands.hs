--------------------------------------------------------------------------------

-- | Responsible for definining and parsing various CLI options/arguments.
--
module Decks.Commands where

import           Options.Applicative

--------------------------------------------------------------------------------

data Opts = Opts
    { optFilepath :: FilePath
    , optOutPath  :: FilePath
    , optWatch    :: Bool
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts =
    Opts
        <$> argument
                str
                (  metavar "PATH"
                <> showDefault
                <> value "index.html"
                <> help "Directory path containing a Decks file to parse."
                )
        <*> strOption
                (long "out" <> metavar "OUTPUT" <> help
                    "Path for the output HTML file"
                )
        <*> (not <$> switch
                (long "no-watch" <> short 'n' <> help
                    "Only run Decks once, not updating on subsequent changes"
                )
            )

--------------------------------------------------------------------------------
