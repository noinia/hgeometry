module RangeSpec where

import Data.Geometry.Properties
import Data.Range
import Frames.CoRec
import Test.Hspec


spec :: Spec
spec = do
  describe "RangeRange Intersection" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    -- it "closed open " $ do
    --   ((ClosedRange 1 10) `intersect` (OpenRange 5 10))
    --   `shouldBe`
    --   (Col (Range (Open 5) (Closed 10)))
            -- encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"

    -- |
  --
  -- >>>
  --
  -- >>> (OpenRange 1 10) `intersect` (OpenRange 5 10)
  -- (Col Range {_lower = Open 5, _upper = Open 10})
  -- >>> (ClosedRange (1::Int) 10) `intersect` (OpenRange 50 (60 :: Int))
  -- (Col NoIntersection)
  -- >>> (OpenRange 1 10) `intersect` (OpenRange 10 12)
  -- (Col NoIntersection)
  -- >>> (OpenRange 1 10) `intersect` (ClosedRange 10 12)
  -- (Col Range {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenRange 1 10) `intersect` (ClosedRange 10 12)
  -- FALSE
