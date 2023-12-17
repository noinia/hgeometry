module HGeometry.IntervalSpec where

import Control.Lens
import Data.Maybe (isJust)
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Kernel.Instances ()
-- import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck

--------------------------------------------------------------------------------


spec :: Spec
spec = do
  describe "interval " $ do
    let test :: ClosedInterval Int
        test = ClosedInterval 5 10
        test' :: ClosedInterval Int
        test' = read "Interval (ClosedE 5) (ClosedE 10)"
    it "show,read" $ test `shouldBe` test'

  describe "Interval_x_Interval Intersection" $ do
    describe "has intersection testing consistent" $ do
      prop "closedInterval and arbitrary Interval"  $
        \(intA :: ClosedInterval Int) (intB :: Interval AnEndPoint Int) ->
          (intA `intersects` intB)
          `shouldBe`
          (asAnInterval intA `intersects` intB)

    prop "intersects and intersect consistent" $
      \(intA :: ClosedInterval Int) (intB :: ClosedInterval Int) ->
        (intA `intersects` intB) == isJust (intA `intersect` intB)


    -- it "openInterval cap openrange" $ do
    --   ((OpenInterval 1 (10 :: Int))  `intersect` (OpenInterval 5 (10 :: Int)))
    --   `shouldBe` (ClosedInterval_x_ClosedInterval_Partial $ OpenInterval 5 (10 :: Int))
    it "openInterval cap openrange" $ do
      ((OpenInterval 1 (10 :: Int))  `intersects` (OpenInterval 5 (10 :: Int)))
      `shouldBe` True
    it "disjoint open ranges" $ do
      ((OpenInterval 1 (10 :: Int)) `intersects` (OpenInterval 10 (12 :: Int)))
      `shouldBe` False
    it "closed cap open, disjoint" $ do
      ((ClosedInterval (1::Int) 10) `intersects` (OpenInterval 50 (60 :: Int)))
      `shouldBe` False
    -- it "endpoints overlap but open/closed" $ do

    it "manual tests " $ do
      mapM_ (\t@((i,j),_) -> ((i,j),testInt i `intersect` testInt j) `shouldBe` t ) answers



      -- let r1, r2 :: ClosedInterval Int
      -- (r1 `intersects` r2) `shouldBe` True
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

    --
  --
  -- >>>
  --
  -- >>>
  -- (Col NoIntersection)
  -- >>> (OpenInterval 1 10) `intersect` (ClosedInterval 10 12)
  -- (Col Interval {_lower = Closed 10, _upper = Open 10})
  -- >>> (OpenInterval 1 10) `intersect` (ClosedInterval 10 12)
  -- FALSE

--------------------------------------------------------------------------------
-- * Some manual intersection tests

testInts :: [ClosedInterval Int]
testInts = [ ClosedInterval 10 20 -- 0
           , ClosedInterval 1 15  -- 1
           , ClosedInterval 20 30 -- 2
           , ClosedInterval 0 20  -- 3
           , ClosedInterval 10 16 -- 4
           ]

testInt   :: Int -> ClosedInterval Int
testInt i = testInts !! i

answers = [ ( (0,1) , Just $ ClosedInterval_x_ClosedInterval_Partial $ ClosedInterval 10 15)
          , ( (0,2) , Just $ ClosedInterval_x_ClosedInterval_Point 20 )
          , ( (0,3) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 0 )
          , ( (0,4) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 4 )
          , ( (1,1) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 1 )
          , ( (1,2) , Nothing )
          , ( (1,3) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 1  )
          , ( (1,4) , Just $ ClosedInterval_x_ClosedInterval_Partial $ ClosedInterval 10 15  )
          , ( (2,2) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 2 )
          , ( (2,3) , Just $ ClosedInterval_x_ClosedInterval_Point 20  )
          , ( (2,4) , Nothing  )
          , ( (3,3) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 3 )
          , ( (3,4) , Just $ ClosedInterval_x_ClosedInterval_Contained $ testInt 4 )
          ]


--------------------------------------------------------------------------------
