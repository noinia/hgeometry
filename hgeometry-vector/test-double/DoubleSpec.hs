module DoubleSpec where

import HGeometry.Vector.Generic
import Vector
-- import HGeometry.Vector.Class -- figure out why we need this?
import Test.Hspec
import R

--------------------------------------------------------------------------------

myVec :: Vector
myVec = Vector2_ 5 11

spec :: Spec
spec = describe "double spec" $ do
         it "showtest" $
           show myVec `shouldBe` "Vector2 5.0 11.0"
         it "division" $
           myVec ^/ 2 `shouldBe` Vector2_ 2.5 5.5
         describe "numerical robustness" $ do
           it "1e0" $
             isScalarMultipleOf (Vector2_ 1 10) (Vector2_ 10 (10*10::R)) `shouldBe` True
             -- With sufficiently large numbers, isScalarMultipleOf fails for Doubles.
           it "1e10 (fail)" $
             isScalarMultipleOf (Vector2_ 1 10) (Vector2_ 1e10 (1e10*10::R)) `shouldBe` False
         -- it "norm" $
         --   norm myVec `shouldBe` 2 -- this is nonsense, but just to verify
         -- SafeDouble should work better.
           -- it "1e10 (pass)" $
           --   isScalarMultipleOf (Vector2 1 10) (Vector2 1e10 (1e10*10::SafeDouble)) `shouldBe` True
