module Vector2Spec where

import Test.Hspec
import HGeometry.Vector.Vector2

myVec :: Vector
myVec = Vector2 5 11

spec :: Spec
spec = describe "vector2 tests" $
         it "showtest" $
           show myVec `shouldBe` "Vector2 5 11"
