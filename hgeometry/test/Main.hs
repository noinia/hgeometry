module Main where

import qualified Spec
import           System.Directory
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = do setCurrentDirectory "../hgeometry"
          hspecWith defaultConfig Spec.spec
