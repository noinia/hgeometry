--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.Diameter.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.Diameter.Naive where

import Data.List (maximumBy)
import Data.Radical
import Geometry.Point

--------------------------------------------------------------------------------

-- | Computes the Euclidean diameter by naively trying all pairs.
--
-- running time: \(O(n^2)\)
diameter :: (Ord r, Radical r, Point_ point d r) => [point d r] -> r
diameter = maybe 0 (uncurry euclideanDist) . diametralPair

-- | Computes the Euclidean diametral pair by naively trying all pairs.
--
-- running time: \(O(n^2)\)
diametralPair :: (Ord r, Num r, Point_ point d r)
              => [point d r] -> Maybe (point d r, point d r)
diametralPair = diametralPairWith squaredEuclideanDist

-- | Given a distance function and a list of points pts, computes the diametral
-- pair by naively trying all pairs.
--
-- running time: \(O(n^2)\)
diametralPairWith               :: Ord r
                                => (point -> point -> r)
                                -> [point]
                                -> Maybe (point, point)
diametralPairWith f pts@(_:_:_) = Just $ maximumBy cmp [ (p,q) | p <- pts, q <- pts ]
  where
    f' (p,q) = f p q
    tp `cmp` tq = f' tp `compare` f' tq
diametralPairWith _ _           = Nothing
