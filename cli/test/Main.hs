--------------------------------------------------------------------------------

module Main where

import qualified CodeGenSpec
import qualified ParserSpec

import           Test.Hspec

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    ParserSpec.spec
    CodeGenSpec.spec

--------------------------------------------------------------------------------
