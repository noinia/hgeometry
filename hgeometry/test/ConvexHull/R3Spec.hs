module ConvexHull.R3Spec (spec) where

-- import           Data.Foldable1
-- import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified HGeometry.ConvexHull.R3.Naive.Dual as Dual
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
-- import           HGeometry.Point
-- import           HGeometry.Triangle
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Instances ()


import Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5


spec = describe "3D convex hull through duality tests " $ do
         pure ()
         -- fit "single triangle" $ do
         --   let tri = Triangle origin (Point3 10 0 1) (Point3 0 10 2 :: Point 3 R)
           -- Dual.facets (traceShowWith ("hull",) $ Dual.upperHull tri) `shouldBe` []
           -- FIXME: This is actually incorrect. I think it should be the thing below:
           -- Dual.facets (traceShowWith ("hull",) $ Dual.upperHull tri) `shouldBe` [toNonEmpty tri]
