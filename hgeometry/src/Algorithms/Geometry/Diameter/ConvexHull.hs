--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.Diameter.ConvexHull
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals, David Himmelstrup
--------------------------------------------------------------------------------
module Algorithms.Geometry.Diameter.ConvexHull
  ( diameter
  , diametralPair
  ) where

import           Algorithms.Geometry.ConvexHull.GrahamScan (convexHull)
import qualified Algorithms.Geometry.Diameter.Naive as Naive
import           Control.Lens ((^.))
import           Data.Ext (core, type (:+))
import qualified Data.List.NonEmpty as NonEmpty
import           Geometry.Point
import qualified Geometry.Polygon.Convex as Convex

--------------------------------------------------------------------------------

-- | Computes the Euclidean diameter by first finding the convex hull.
--
-- running time: \(O(n \log n)\)
diameter :: (Ord r, Floating r) => [Point 2 r :+ p] -> r
diameter = maybe 0 (\(p,q) -> euclideanDist (p^.core) (q^.core)) . diametralPair

-- | Computes the Euclidean diameter by first finding the convex hull.
--
-- running time: \(O(n \log n)\)
diametralPair :: (Ord r, Num r)
                   => [Point 2 r :+ p] -> Maybe (Point 2 r :+ p, Point 2 r :+ p)
diametralPair lst@(_:_:_:_) = Just . Convex.diametralPair $ convexHull $ NonEmpty.fromList lst
diametralPair lst           = Naive.diametralPair lst
