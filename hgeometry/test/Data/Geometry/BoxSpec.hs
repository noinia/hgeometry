module Data.Geometry.BoxSpec where

import Data.Geometry
import Data.Geometry.Box
import Data.Geometry.Properties
import Test.Hspec


spec :: Spec
spec = do
  describe "Box" $ do
    it "intersect tests" $
      ((boundingBoxList' $ [point2 (-4) (-3), point2 (-4) (10 :: Int)])
       `intersects`
       (boundingBoxList' $ [point2 (-5) 1, point2 (-4) (0 :: Int)]))
      `shouldBe` True
