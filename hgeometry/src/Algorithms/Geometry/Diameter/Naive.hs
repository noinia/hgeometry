module Algorithms.Geometry.Diameter.Naive where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.List(maximumBy)

--------------------------------------------------------------------------------

diameter :: (Ord r, Floating r, Arity d) => [Point d r :+ p] -> r
diameter = maybe 0 (\(p,q) -> euclideanDist (p^.core) (q^.core)) . diametralPair

-- | Computes the Euclidean diametral pair by naively trying all pairs.
--
-- running time: \(O(n^2)\)
diametralPair :: (Ord r, Num r, Arity d)
                   => [Point d r :+ p] -> Maybe (Point d r :+ p, Point d r :+ p)
diametralPair = diametralPairWith squaredEuclideanDist

-- | Given a distance function and a list of points pts, computes the diametral
-- pair by naively trying all pairs.
--
-- running time: \(O(n^2)\)
diametralPairWith               :: Ord r
                                     => (Point d r -> Point d r -> r)
                                     -> [Point d r :+ p]
                                     -> Maybe (Point d r :+ p, Point d r :+ p)
diametralPairWith f pts@(_:_:_) = Just $ maximumBy cmp [ (p,q) | p <- pts, q <- pts ]
  where
    f' (p,q) = f (p^.core) (q^.core)
    tp `cmp` tq = f' tp `compare` f' tq
diametralPairWith _ _           = Nothing
