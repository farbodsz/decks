--------------------------------------------------------------------------------

-- | Responsible for definining and parsing various CLI options/arguments.
--
module Decks.Commands where

import           Options.Applicative

--------------------------------------------------------------------------------

data Opts = Opts
    { optFilepath :: FilePath
    -- , optWatch    :: Bool
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts =
    Opts
        <$> (argument
                str
                (metavar "FILEPATH" <> help "Path for the Decks file to parse.")
            )
        -- <*> switch
        --         (long "watch" <> short 'w' <> help
        --             "Watches file and updates on changes."
        --         )

--------------------------------------------------------------------------------
