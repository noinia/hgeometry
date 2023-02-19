module IntSpec where

import HGeometry.Vector
import HGeometry.Point
import R
import Test.Hspec

--------------------------------------------------------------------------------

myVec :: Vector 2 R
myVec = Vector2 5 11

myPoint :: Point 2 R
myPoint = Point2 5 11

spec :: Spec
spec = describe "Int vector spec" $ do
         it "showtest" $
           show myPoint `shouldBe` "Point2 5 11"
         -- it "division" $
         --   myVec ^/ 2 `shouldBe` Vector2 2 5 -- this should not compile :)
