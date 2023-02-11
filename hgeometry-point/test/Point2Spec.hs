module Point2Spec where

import Control.Lens
import HGeometry.Point
import HGeometry.Point.PointF
import HGeometry.Vector
import HGeometry.Vector.Vector2
import HGeometry.Properties
import Test.Hspec

--------------------------------------------------------------------------------

myPoint :: Point
myPoint = Point2 5 11

type R = NumType Point

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2       :: R -> R -> Point
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

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
