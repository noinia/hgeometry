{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polygon.Convex.UnboundedSpec
  (spec
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.ConvexHull.GrahamScan (convexHull)
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import qualified HGeometry.Polygon.Convex.Merge as Merge
import           HGeometry.Polygon.Convex.MinkowskiSum
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Transformation
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 10

--------------------------------------------------------------------------------

spec :: Spec
spec = do describe "Polygon.Convex.Unbounded"
            let unboundeds = []
            let draw region =
                   foldMap (\h -> [ipeHalfPlane h]) (unboundedBoundingHalfplanes region)
                   <>
                   defIO region
            goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
               (ipeContentGolden { name = [osp|unboundedBoundingHalfplanes|]
                                 }
               )
               (foldMap draw unboundeds)
