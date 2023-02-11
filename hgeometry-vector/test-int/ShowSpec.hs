module ShowSpec where

import HGeometry.Vector.Vector2
import Test.Hspec

myVec :: Vector
myVec = Vector2 5 11

spec :: Spec
spec = it "showtest" $
           show myVec `shouldBe` "Vector2 5 11"
