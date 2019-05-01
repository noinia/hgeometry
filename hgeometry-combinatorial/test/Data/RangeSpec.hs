module Data.RangeSpec where

import Data.Intersection
import Data.Range
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "RangeRange Intersection" $ do
    it "openRange cap openrange" $ do
      ((OpenRange 1 (10 :: Int))  `intersect` (OpenRange 5 (10 :: Int)))
      `shouldBe` (coRec $ OpenRange 5 (10 :: Int))
    it "disjoint open ranges" $ do
      ((OpenRange 1 (10 :: Int)) `intersect` (OpenRange 10 (12 :: Int)))
      `shouldBe` (coRec NoIntersection)
    it "closed cap open, disjoint" $ do
      ((ClosedRange (1::Int) 10) `intersect` (OpenRange 50 (60 :: Int)))
      `shouldBe` (coRec NoIntersection)
    -- it "closed intersect open" $
    --   ((OpenRange 1 (10 :: Int)) `intersect` (ClosedRange 10 (12 :: Int)))
    --   `shouldBe` (coRec NoIntersection)

    -- it "open rage intersect closed " $ do
    --   ((OpenRange 1 (10 :: Int)) `intersect` (ClosedRange 10 (12 :: Int)))
    --   `shouldBe` (coRec $ Range (Open 10) (Open (10 :: Int)))
  -- (Col Range {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenRange 1 10) `intersect` (ClosedRange 10 12)


    -- it "closed open " $ do
    --   ((ClosedRange 1 10) `intersect` (OpenRange 5 10))
    --   `shouldBe`
    --   (Col (Range (Open 5) (Closed 10)))
            -- encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"

    -- |
  --
  -- >>>
  --
  -- >>>
  -- (Col NoIntersection)
  -- >>> (OpenRange 1 10) `intersect` (ClosedRange 10 12)
  -- (Col Range {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenRange 1 10) `intersect` (ClosedRange 10 12)
  -- FALSE
