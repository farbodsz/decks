--------------------------------------------------------------------------------

-- | Responsible for definining and parsing various CLI options/arguments.
--
module Decks.Commands where

import           Decks.Document                 ( DecksDocument(DecksDocument)
                                                , HtmlOutput(HtmlOutput)
                                                )
import           Decks.Utils                    ( URL )
import           Options.Applicative

--------------------------------------------------------------------------------

data Opts = Opts
    { optDslPath     :: DecksDocument
    , optOutPath     :: HtmlOutput
    , optFrontendUrl :: URL
    , optWatch       :: Bool
    , optVerbose     :: Bool
    }

parseCmd :: IO Opts
parseCmd = execParser $ info (pOpts <**> helper) (fullDesc <> header "Decks")

pOpts :: Parser Opts
pOpts =
    Opts
        <$> (DecksDocument <$> strOption
                (  long "input"
                <> metavar "INPUT_FILE"
                <> showDefault
                <> value defaultInputFile
                <> help "File path of the Decks DSL file to parse"
                )
            )
        <*> (HtmlOutput <$> strOption
                (  long "output"
                <> metavar "OUTPUT_FILE"
                <> showDefault
                <> value defaultOutputFile
                <> help "Destination of the output HTML file"
                )
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

defaultInputFile :: FilePath
defaultInputFile = "./presentation.decks"

defaultOutputFile :: FilePath
defaultOutputFile = "index.html"

defaultFrontendUrl :: URL
defaultFrontendUrl = "http://localhost:3000"

--------------------------------------------------------------------------------
