--------------------------------------------------------------------------------

-- | Responsible for definining and parsing various CLI options/arguments.
--
module Decks.Commands where

import           Decks.Utils                    ( URL )

import           Options.Applicative

--------------------------------------------------------------------------------

data Opts = Opts
    { optDirPath     :: FilePath
    , optOutPath     :: FilePath
    , optFrontendUrl :: URL
    , optWatch       :: Bool
    , optVerbose     :: Bool
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts =
    Opts
        <$> strOption
                (  long "input"
                <> metavar "INPUT_DIR"
                <> showDefault
                <> value defaultInputDir
                <> help "Directory path containing a Decks file to parse"
                )
        <*> strOption
                (  long "output"
                <> metavar "OUTPUT_FILE"
                <> showDefault
                <> value defaultOutputFile
                <> help "Destination of the output HTML file"
                )
        <*> strOption
                (  long "frontend"
                <> metavar "FRONTEND_URL"
                <> showDefault
                <> value defaultFrontendUrl
                <> help "URL of the frontend, the web WYSIWYG editor"
                )
        <*> (not <$> switch
                (long "no-watch" <> short 'n' <> help
                    "Only run Decks once, not updating on subsequent changes"
                )
            )
        <*> switch
                (long "verbose" <> short 'v' <> help
                    "Verbose mode prints the generated AST and HTML"
                )

--------------------------------------------------------------------------------

defaultInputDir :: FilePath
defaultInputDir = "."

defaultOutputFile :: FilePath
defaultOutputFile = "index.html"

defaultFrontendUrl :: URL
defaultFrontendUrl = "http://localhost:3000"

--------------------------------------------------------------------------------
