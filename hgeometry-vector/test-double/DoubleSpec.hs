module DoubleSpec where

import HGeometry.Vector.Vector2
import HGeometry.Vector.Additive
import Test.Hspec

myVec :: Vector
myVec = Vector2 5 11

spec :: Spec
spec = describe "double spec" $ do
         it "showtest" $
           show myVec `shouldBe` "Vector2 5.0 11.0"
         it "division" $
           myVec ^/ 2 `shouldBe` Vector2 2.5 5.5
