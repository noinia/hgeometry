module Geometry.Point.EuclideanDistance
  ( cmpByDistanceTo
  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)
  ) where


import           Data.Ord (comparing)
import qualified Data.Radical as Radical
import           Geometry.Point.Class
import           Geometry.Properties
import           Geometry.Vector

--------------------------------------------------------------------------------
-- * Distances

-- | Squared Euclidean distance between two points
squaredEuclideanDist :: (Num r, Arity d, Point_ point d r) => point d r -> point d r -> r
squaredEuclideanDist = qdA

-- | Euclidean distance between two points
euclideanDist     :: (Radical.Radical r, Arity d, Point_ point d r) => point d r -> point d r -> r
euclideanDist p q = Radical.sqrt $ squaredEuclideanDist p q

-- | Compare two points by their distance to the first argument
cmpByDistanceTo   :: (Ord r, Num r, Arity d, Point_ point d r)
                  => point d r -> point d r -> point d r -> Ordering
cmpByDistanceTo c = comparing (squaredEuclideanDist c)

--------------------------------------------------------------------------------

class HasSquaredEuclideanDistance g where
  {-# MINIMAL pointClosestToWithDistance | pointClosestTo #-}
  -- | Given a point q and a geometry g, the squared Euclidean distance between q and g.
  squaredEuclideanDistTo   :: ( Num (NumType g)
                              , Point_ point (Dimension  g) (NumType g))
                           => point (Dimension g) (NumType g) -> g -> NumType g
  squaredEuclideanDistTo q = snd . pointClosestToWithDistance q

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance.
  pointClosestTo   :: (Num (NumType g), Point_ point (Dimension g) (NumType g))
                   => point (Dimension g) (NumType g) -> g
                   -> point (Dimension g) (NumType g)
  pointClosestTo q = fst . pointClosestToWithDistance q

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance. Returns both the point and the
  -- distance realized by this point.
  pointClosestToWithDistance     :: (Num (NumType g), Point_ point (Dimension g) (NumType g))
                                 => point (Dimension g) (NumType g) -> g
                                 -> (point (Dimension g) (NumType g), NumType g)
  pointClosestToWithDistance q g = let p = pointClosestTo q g
                                   in (p, squaredEuclideanDist p q)
