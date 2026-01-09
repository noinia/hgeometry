module Plane.RandomizedEnvSpec
  where

import Plane.Randomized2
import Test.Hspec
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import R

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Plane.RandomizedEnvSpec" $ do
         it "fail" $
           5 `shouldBe` (6 :: Int)
