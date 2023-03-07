module Main (main) where

import qualified Spec
import           Test.Hspec.Runner
-- import           Test.Hspec

main :: IO ()
main = hspecWith defaultConfig Spec.spec
