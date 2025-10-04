{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the convex polygon types in hgeometry
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Instances
  (
  ) where

import Control.Lens
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.ConvexHull
import HGeometry.Point
import HGeometry.Triangle
import HGeometry.Kernel.Instances ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty

--------------------------------------------------------------------------------

instance (Arbitrary point, Point_ point 2 r, Ord r, Num r, Eq point
         ) => Arbitrary (ConvexPolygon point) where
  arbitrary = do Triangle a b c <- arbitrary
                 rest           <- arbitrary
                 pure $ convexHull (a :| b : c : rest)
    -- I guess we may end up having still relatively few vertices on the CH


instance (Arbitrary point, Point_ point 2 r, Ord r, Num r, Eq point
         ) => Arbitrary (UnboundedConvexRegionF r NonEmpty point) where
  arbitrary = arbitrary <&> \(ch :: ConvexPolygon point) ->
    let vs@((p, (prev',_)) :| _) = toNonEmptyOf outerBoundaryWithNeighbours ch
        v                        = p .-. prev'
        stillDiverging (c,(_,q)) = ccw (c .-^ v) c q == CCW
    in case NonEmpty.nonEmpty $ NonEmpty.takeWhile stillDiverging vs of
         Nothing  -> error "UnBoundedConvexRegion.arbitrary; this should be impossible"
                     -- Unbounded    v (p :| [])                      (next' .-. p)
         Just pts -> let (q,(_,nextQ)) = NonEmpty.last pts
                     in Unbounded v (fst <$> pts) (nextQ .-. q)
