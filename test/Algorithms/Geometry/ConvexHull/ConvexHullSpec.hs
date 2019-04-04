module Algorithms.Geometry.ConvexHull.ConvexHullSpec where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan
import           Control.Lens
import           Data.CircularSeq (isShiftOf)
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances()
import           Test.QuickCheck.Instances()


spec :: Spec
spec = do
    describe "ConvexHull Algorithms" $ do
      modifyMaxSize (const 1000) . modifyMaxSuccess (const 1000) $
        it "GrahamScan and DivideAnd Conquer are the same" $
          property $ \pts ->
            (PG $ GrahamScan.convexHull pts)
            ==
            (PG $ DivideAndConquer.convexHull pts)

newtype PG = PG (ConvexPolygon () Rational) deriving (Show)

instance Eq PG where
  (PG a) == (PG b) = let as = a^.simplePolygon.outerBoundary
                         bs = b^.simplePolygon.outerBoundary
                     in isShiftOf as bs
