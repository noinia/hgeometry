module Algorithms.Geometry.WeightedClosestPair.Naive where

import Control.Lens
import Data.Ext
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.UnBounded
import Data.Util


-- | Squared weighted distance between two disks p and q, i.e. the square of the scaling t
-- such that p*t and q*t touch in a point.
sqWDist                         :: Fractional r => Disk p r -> Disk p r -> r
sqWDist (Disk p wp) (Disk q wq) = let d = squaredEuclideanDist (p^.core) (q^.core)
                                      w = wp + wq
                                  in  d / (w*w)

-- | Computes the square weighted closest pair distance in a brute force manner.
-- returns Top (i.e. +infty) if we have only one or two disks.
--
-- running time: \(O(n^2)\)
sqWeightedClosestPair :: (Fractional r, Ord r) => [Disk p r] -> Top r
sqWeightedClosestPair = \case
    []  -> Top
    [_] -> Top
    sds -> ValT . minimum . map (\(SP p q) -> sqWDist p q) . uniquePairs $ sds
