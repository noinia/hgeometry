module Algorithms.Geometry.ConvexHull.ConvexHullSpec where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConqueror as DivideAndConqueror
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan
import           Control.Lens
import           Data.CircularSeq (isShiftOf)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances
import           Util

import           Debug.Trace

spec :: Spec
spec = do
    describe "ConvexHull Algorithms" $ do
      modifyMaxSize (const 1000) . modifyMaxSuccess (const 1000) $
        it "GrahamScan and DivideAnd Conqueror are the same" $
          property $ \pts ->
            (PG $ GrahamScan.convexHull pts)
            ==
            (PG $ DivideAndConqueror.convexHull pts)

newtype PG = PG (ConvexPolygon () Rational) deriving (Show)

instance Eq PG where
  (PG a) == (PG b) = let as = a^.simplePolygon.outerBoundary
                         bs = b^.simplePolygon.outerBoundary
                     in isShiftOf as bs
