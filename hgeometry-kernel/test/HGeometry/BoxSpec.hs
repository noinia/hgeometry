module HGeometry.BoxSpec where

import Control.Lens
import Data.Ext
import Data.Ratio
import HGeometry.Box
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec
import Test.QuickCheck
-- import Test.Util

--------------------------------------------------------------------------------

-- arbitraryPointInBoundingBox   :: Box (Point 2 Rational) -> Gen (Point 2 Rational)
-- arbitraryPointInBoundingBox b = do
--   ZeroToOne rX <- arbitrary
--   ZeroToOne rY <- arbitrary
--   let minPt        = b^.minPoint
--       offsetVector = Vector2 (width b * rX) (height b * rY)
--   pure $ minPt .+^ offsetVector

spec :: Spec
spec = pure ()

-- spec :: Spec
-- spec = do
--   describe "Box" $ do
--     it "intersect tests" $
--       ((boundingBoxList' $ [Point2 (-4) (-3), Point2 (-4) (10 :: Int)])
--        `intersects`
--        (boundingBoxList' $ [Point2 (-5) 1, Point2 (-4) (0 :: Int)]))
--       `shouldBe` True


newtype ZeroToOne = ZeroToOne Rational

instance Show ZeroToOne where
  show (ZeroToOne r) = show r

instance Arbitrary ZeroToOne where
  arbitrary = do
    k <- chooseInteger (0, granularity)
    pure $ ZeroToOne $ k % granularity
    where
      granularity = 1000000
  shrink (ZeroToOne 1) = []
  shrink (ZeroToOne 0) = []
  shrink (ZeroToOne r) = [ ZeroToOne $ div (numerator r) 2 % div (denominator r) 2]
