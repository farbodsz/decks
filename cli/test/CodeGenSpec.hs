--------------------------------------------------------------------------------

module CodeGenSpec
    ( spec
    ) where

import           Control.Monad                  ( forM_ )
import           Control.Monad.Trans.State      ( evalStateT )
import           Data.Either.Extra              ( eitherToMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Decks.Compiler.CodeGen.Generate
                                                ( genProgram )
import           Decks.Compiler.CodeGen.Types   ( initDecksStore )
import           Decks.Compiler.Parser          ( pProgram )
import           System.Directory               ( getDirectoryContents )
import           System.FilePath                ( takeBaseName )
import           Test.Hspec
import           Text.Megaparsec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "code generation" testFiles

testFiles :: Spec
testFiles = describe "test files" $ do
    let inputsDir  = "test/res/input/"
        outputsDir = "test/res/output/"
        inputFpath t = inputsDir ++ t ++ ".decks"
        outputFpath t = outputsDir ++ t ++ ".html"

    _fileNames <- runIO $ getDirectoryContents inputsDir
    let tests = map takeBaseName . filter (`notElem` [".", ".."]) $ _fileNames

    forM_ tests $ \test -> do
        it ("file: " ++ test) $ do
            input  <- TIO.readFile $ inputFpath test
            output <- TIO.readFile $ outputFpath test
            parseThenGen test input `shouldBe` Just output

-- TODO: move to src?
-- | Returns the generated code from the input Decks file, or Nothing if an
-- error occurred.
parseThenGen :: FilePath -> Text -> Maybe Text
parseThenGen path contents = do
    ast <- eitherToMaybe $ runParser pProgram path contents
    eitherToMaybe $ evalStateT (genProgram ast) initDecksStore

--------------------------------------------------------------------------------
