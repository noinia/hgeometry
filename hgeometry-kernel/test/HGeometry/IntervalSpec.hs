module HGeometry.IntervalSpec where

import HGeometry.Intersection
import HGeometry.Interval
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Interval_x_Interval Intersection" $ do
    -- it "openInterval cap openrange" $ do
    --   ((OpenInterval 1 (10 :: Int))  `intersect` (OpenInterval 5 (10 :: Int)))
    --   `shouldBe` (ClosedInterval_x_ClosedInterval_Partial $ OpenInterval 5 (10 :: Int))
    -- it "disjoint open ranges" $ do
    --   ((OpenInterval 1 (10 :: Int)) `intersect` (OpenInterval 10 (12 :: Int)))
    --   `shouldBe` (coRec NoIntersection)
    -- it "closed cap open, disjoint" $ do
    --   ((ClosedInterval (1::Int) 10) `intersect` (OpenInterval 50 (60 :: Int)))
    --   `shouldBe` (coRec NoIntersection)
    -- it "endpoints overlap but open/closed" $ do

    it "manual " $ do
      let r1, r2 :: ClosedInterval Int
          r1 = ClosedInterval 3 6
          r2 = ClosedInterval 1 3 --Interval (Open 1) (Open 3)
      (r1 `intersects` r2) `shouldBe` True
    -- it "closed intersect open" $
    --   ((OpenInterval 1 (10 :: Int)) `intersect` (ClosedInterval 10 (12 :: Int)))
    --   `shouldBe` (coRec NoIntersection)

    -- it "open rage intersect closed " $ do
    --   ((OpenInterval 1 (10 :: Int)) `intersect` (ClosedInterval 10 (12 :: Int)))
    --   `shouldBe` (coRec $ Interval (Open 10) (Open (10 :: Int)))
  -- (Col Interval {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenInterval 1 10) `intersect` (ClosedInterval 10 12)


    -- it "closed open " $ do
    --   ((ClosedInterval 1 10) `intersect` (OpenInterval 5 10))
    --   `shouldBe`
    --   (Col (Interval (Open 5) (Closed 10)))
            -- encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"

    -- |
  --
  -- >>>
  --
  -- >>>
  -- (Col NoIntersection)
  -- >>> (OpenInterval 1 10) `intersect` (ClosedInterval 10 12)
  -- (Col Interval {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenInterval 1 10) `intersect` (ClosedInterval 10 12)
  -- FALSE
