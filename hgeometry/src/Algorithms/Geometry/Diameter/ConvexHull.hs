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
import           Data.Radical
import           Geometry.Point
import qualified Geometry.Polygon.Convex as Convex

--------------------------------------------------------------------------------

-- | Computes the Euclidean diameter by first finding the convex hull.
--
-- running time: \(O(n \log n)\)
diameter :: (Ord r, Radical r, Point_ point 2 r) => [point 2 r] -> r
diameter = maybe 0 (uncurry euclideanDist) . diametralPair

-- | Computes the Euclidean diameter by first finding the convex hull.
--
-- running time: \(O(n \log n)\)
diametralPair :: (Ord r, Num r, Point_ point 2 r) => [point 2 r] -> Maybe (point 2 r, point 2 r)
diametralPair lst@(_:_:_:_) = error "diametralPair: temporary"
  --  Just . Convex.diametralPair $ convexHull $ NonEmpty.fromList lst
diametralPair lst           = Naive.diametralPair lst
