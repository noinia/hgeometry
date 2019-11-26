module Data.Geometry.BoxSpec where

import Data.Geometry
import Data.Geometry.Box
import Data.Geometry.Properties
import Test.Hspec


spec :: Spec
spec = do
  describe "Box" $ do
    it "intersect tests" $
      ((boundingBoxList' $ [Point2 (-4) (-3), Point2 (-4) (10 :: Int)])
       `intersects`
       (boundingBoxList' $ [Point2 (-5) 1, Point2 (-4) (0 :: Int)]))
      `shouldBe` True
