module Main (main) where

import qualified Spec
import           System.Directory
import           Test.Hspec.Runner
-- import           Test.Hspec

main :: IO ()
main = do setCurrentDirectory "../hgeometry"
          hspecWith defaultConfig Spec.spec
