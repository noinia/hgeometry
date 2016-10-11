module Algorithms.Geometry.Diameter where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.List(maximumBy)

--------------------------------------------------------------------------------


diameterNaive :: (Ord r, Floating r, Arity d) => [Point d r :+ p] -> r
diameterNaive = maybe 0 (\(p,q) -> euclideanDist (p^.core) (q^.core))
              . diametralPairNaive

-- | Computes the Euclidean diametral pair by naively trying all pairs.
--
-- running time: $O(n^2)$
diametralPairNaive :: (Ord r, Num r, Arity d)
                   => [Point d r :+ p] -> Maybe (Point d r :+ p, Point d r :+ p)
diametralPairNaive = diametralPairWithNaive squaredEuclideanDist


-- | Given a distance function and a list of points pts, computes the diametral
-- pair by naively trying all pairs.
--
-- running time: $O(n^2)$
diametralPairWithNaive               :: Ord r
                                     => (Point d r -> Point d r -> r)
                                     -> [Point d r :+ p]
                                     -> Maybe (Point d r :+ p, Point d r :+ p)
diametralPairWithNaive f pts@(_:_:_) = Just $ maximumBy cmp [ (p,q) | p <- pts, q <- pts ]
  where
    f' (p,q) = f (p^.core) (q^.core)
    tp `cmp` tq = f' tp `compare` f' tq
diametralPairWithNaive _ _           = Nothing
