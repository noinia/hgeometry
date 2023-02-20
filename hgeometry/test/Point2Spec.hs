module Point2Spec where

import Control.Lens
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec
import R

--------------------------------------------------------------------------------

myPoint :: Point 2 R
myPoint = Point2 5 11

spec :: Spec
spec = describe "point2 tests" $ do
         it "coordinatess" $
           myPoint^..coordinates `shouldBe` [5,11]
         it "coordinates" $ do
           myPoint^.(coord @1) `shouldBe` 5
           myPoint^.(coord @2) `shouldBe` 11
         it "xCoord" $
           myPoint^.xCoord `shouldBe` 5
         it "yCoord" $
           myPoint^.yCoord `shouldBe` 11
         it "add" $
           myPoint .+^ Vector2 5 11 `shouldBe` Point2 10 22
