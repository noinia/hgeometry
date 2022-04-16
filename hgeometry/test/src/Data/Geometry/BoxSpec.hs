module Data.Geometry.BoxSpec where

import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import           Test.Hspec
import           Test.QuickCheck
import           Test.Util

arbitraryPointInBoundingBox :: Box 2 p Rational -> Gen (Point 2 Rational)
arbitraryPointInBoundingBox b = do
  ZeroToOne rX <- arbitrary
  ZeroToOne rY <- arbitrary
  let minPt = minPoint b ^. core
      offsetVector = Vector2 (width b * rX) (height b * rY)
  pure $ minPt .+^ offsetVector

spec :: Spec
spec = do
  describe "Box" $ do
    it "intersect tests" $
      ((boundingBoxList' $ [Point2 (-4) (-3), Point2 (-4) (10 :: Int)])
       `intersects`
       (boundingBoxList' $ [Point2 (-5) 1, Point2 (-4) (0 :: Int)]))
      `shouldBe` True
