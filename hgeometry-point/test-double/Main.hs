module Main (main) where

import qualified Spec
import qualified GenericSpec
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig (GenericSpec.spec >> Spec.spec)
