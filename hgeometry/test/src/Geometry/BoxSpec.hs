module Geometry.BoxSpec where

import Control.Lens
import Data.Ext
import Data.Intersection
import Geometry.Box
import Geometry.Point
import Geometry.Vector
import Test.Hspec
import Test.QuickCheck
import Test.Util

arbitraryPointInBoundingBox :: Box 2 p Rational -> Gen (Point 2 Rational)
arbitraryPointInBoundingBox b = do
  ZeroToOne rX <- arbitrary
  ZeroToOne rY <- arbitrary
  let minPt        = b^.minPoint.core
      offsetVector = Vector2 (width b * rX) (height b * rY)
  pure $ minPt .+^ offsetVector

spec :: Spec
spec = do
  describe "Box" $ do
    it "intersect tests" $
      ((boundingBoxList' [Point2 @Point (-4) (-3), Point2 (-4) (10 :: Int)])
       `intersects`
       (boundingBoxList' [Point2 @Point (-5) 1, Point2 (-4) (0 :: Int)]))
      `shouldBe` True
