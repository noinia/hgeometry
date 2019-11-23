module Algorithms.Geometry.ConvexHull.ConvexHullSpec where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan
import qualified Algorithms.Geometry.ConvexHull.JarvisMarch as JarvisMarch
import qualified Algorithms.Geometry.ConvexHull.OldDivideAndConquer as OldDivAndConquer
import qualified Algorithms.Geometry.ConvexHull.QuickHull as QuickHull
import           Control.Lens
import           Data.CircularSeq (isShiftOf)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Set as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck(property)
import           Test.QuickCheck.Instances ()
-- import           Util

import           Debug.Trace

spec :: Spec
spec = do
    describe "ConvexHull Algorithms" $ do
      modifyMaxSize (const 1000) . modifyMaxSuccess (const 1000) $ do
        describe "GrahamScan and DivideAnd Conquer are the same" $ do
          it "quickcheck convex hull " $
            property $ \pts ->
              (PG $ GrahamScan.convexHull pts) == (PG $ DivideAndConquer.convexHull pts)
          it "quickcheck upper hull " $
            property $ \(pts :: NonEmpty (Point 2 Int :+ ())) ->
              GrahamScan.upperHull pts == DivideAndConquer.upperHull pts
          -- it "quickcheck lower hull " $
          --   property $ \(pts :: NonEmpty (Point 2 Int :+ ())) ->
          --     GrahamScan.lowerHull pts == DivideAndConquer.lowerHull pts
          it "manual" $
            (PG $ GrahamScan.convexHull myPoints) == (PG $ DivideAndConquer.convexHull myPoints)

        describe "GrahamScan and Old DivideAnd Conquer are the same" $ do
          -- it "quickcheck " $
          --   property $ \pts ->
          --     (PG $ GrahamScan.convexHull pts) == (PG $ DivideAndConquer.convexHull pts)
          it "manual" $
            (PG $ GrahamScan.convexHull myPoints) == (PG $ OldDivAndConquer.convexHull myPoints)

        -- it "GrahamScan and QuickHull are the same" $
        --   property $ \pts ->
        --     (PG $ GrahamScan.convexHull pts) == (PG $ QuickHull.convexHull pts)

        -- it "GrahamScan and JarvisMarch are the same" $
        --   property $ \pts ->
        --     (PG $ GrahamScan.convexHull pts) == (PG $ JarvisMarch.convexHull pts)






newtype PG = PG (ConvexPolygon () Rational) deriving (Show)

instance Eq PG where
  (PG a) == (PG b) = let as = a^.simplePolygon.outerBoundary
                         bs = b^.simplePolygon.outerBoundary
                     in isShiftOf as bs

myPoints :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
myPoints = NonEmpty.fromList . map ext $
           [ point2 1  3
           , point2 4  26
           , point2 5  17
           , point2 6  7
           , point2 12 16
           , point2 19 4
           , point2 20 0
           , point2 20 11
           , point2 23 23
           , point2 31 14
           , point2 33 5
           ]
