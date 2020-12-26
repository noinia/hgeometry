--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.OldDivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time divide and conquer algorithm to compute the convex hull
-- of a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.OldDivideAndConquer(convexHull) where

import           Algorithms.DivideAndConquer
import           Control.Lens ((^.))
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using divide and conquer. The resulting polygon is
-- given in clockwise order.
convexHull :: (Ord r, Num r)
           => NonEmpty.NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull = unMerge
           . divideAndConquer1 (Merge . ConvexPolygon . fromPoints . (:[]))
           . NonEmpty.sortBy (compare `on` (^.core))

newtype Merge r p = Merge { unMerge :: ConvexPolygon p r }

instance (Num r, Ord r) => Semigroup (Merge r p) where
  (Merge lp) <> (Merge rp) = let (ch,_,_) = merge lp rp in Merge ch
