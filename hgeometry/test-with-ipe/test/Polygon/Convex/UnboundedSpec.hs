{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polygon.Convex.UnboundedSpec
  (spec
  ) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.List.NonEmpty qualified as NonEmpty
import Golden
import HGeometry.ConvexHull.GrahamScan (convexHull)
import HGeometry.Cyclic
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Convex
import HGeometry.Polygon.Convex.Instances()
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 10

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Polygon.Convex.Unbounded" $ do
         unboundeds <- runIO $ sample' $ arbitrary @(UnboundedConvexRegion (Point 2 R))
         let draw region = foldMap (\h -> [iO $ ipeHalfPlane red h])
                                   (unboundedBoundingHalfplanes region)
                           <>
                           [iO $ defIO region]
         goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
               (ipeContentGolden { name = [osp|unboundedBoundingHalfplanes|]
                                 }
               )
               (foldMap draw unboundeds)
