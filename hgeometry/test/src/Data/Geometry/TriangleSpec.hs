{-# LANGUAGE OverloadedStrings #-}
module Geometry.TriangleSpec (spec) where

import Control.Lens
import Data.Ext
import Geometry
import Geometry.Boundary
import Geometry.Triangle
import Data.Proxy
import Data.RealNumber.Rational
import Paths_hgeometry
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 10

spec :: Spec
spec = do describe "intersection tests" $ do
            it "intersecting Line 2 with Triangle 2 " $ do
              let t :: Triangle 2 () Rational
                  t = Triangle (ext origin) (ext $ Point2 10 0) (ext $ Point2 10 10)
                  hor :: Rational -> Line 2 Rational
                  hor = horizontalLine
              (hor 3 `intersect` t)
                `shouldBe` (coRec $ ClosedLineSegment (ext $ Point2 10 (3 :: Rational))
                                                      (ext $ Point2 3  (3 :: Rational)))
              (hor 10 `intersect` t)
                `shouldBe` (coRec $ Point2 10 (10 :: Rational))
              (hor 11 `intersect` t)
                `shouldBe` (coRec NoIntersection)
          it "inTriangle same as inTriangleFrac" $ property $ \q (t :: Triangle 2 () R) ->
              (q `inTriangle` t) `shouldBe` (q `inTriangleFrac` t)
          it "onTriangle same as onTriangleFrac" $ property $ \q (t :: Triangle 2 () R) ->
              (q `onTriangle` t) `shouldBe` (q `onTriangleFrac` t)
          -- TODO: this test probably does not produce many onBoundaries

--------------------------------------------------------------------------------

inTriangleFrac     :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> PointLocationResult
inTriangleFrac q t
    | all (`inRange` OpenRange   0 1) [a,b,c] = Inside
    | all (`inRange` ClosedRange 0 1) [a,b,c] = OnBoundary
    | otherwise                                 = Outside
  where
    Vector3 a b c = toBarricentric q t

-- | Test if a point lies inside or on the boundary of a triangle
onTriangleFrac       :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> Bool
q `onTriangleFrac` t = let Vector3 a b c = toBarricentric q t
                       in all (`inRange` ClosedRange 0 1) [a,b,c]
