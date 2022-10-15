module HGeometry.Point.EuclideanDistance
  ( cmpByDistanceTo
  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)
  ) where


import           Data.Ord (comparing)
import qualified Data.Radical as Radical
import           HGeometry.Point.Class
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * Distances

-- | Squared Euclidean distance between two points
squaredEuclideanDist     :: (Num r, Point_ point d r) => point -> point -> r
squaredEuclideanDist p q = quadrance $ p .-. q

-- | Euclidean distance between two points
euclideanDist     :: (Radical.Radical r, Point_ point d r) => point -> point -> r
euclideanDist p q = Radical.sqrt $ squaredEuclideanDist p q

-- | Compare two points by their distance to the first argument
cmpByDistanceTo   :: (Ord r, Num r, Point_ point d r)
                  => point -> point -> point -> Ordering
cmpByDistanceTo c = comparing (squaredEuclideanDist c)

--------------------------------------------------------------------------------

class HasSquaredEuclideanDistance g where
  {-# MINIMAL pointClosestToWithDistance | pointClosestTo #-}
  -- | Given a point q and a geometry g, the squared Euclidean distance between q and g.
  squaredEuclideanDistTo   :: ( Num (NumType g)
                              , Point_ point (Dimension  g) (NumType g))
                           => point -> g -> NumType g
  squaredEuclideanDistTo q = snd . pointClosestToWithDistance' q
    where
      pointClosestToWithDistance' :: Point_ point' (Dimension g) (NumType g)
                                  => point' -> g -> (point', NumType g)
      pointClosestToWithDistance' = pointClosestToWithDistance


  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance.
  pointClosestTo   :: ( Num (NumType g)
                      , Point_ point    (Dimension g) (NumType g)
                      , Point_ outPoint (Dimension g) (NumType g)
                      )
                   => point -> g -> outPoint
  pointClosestTo q = fst . pointClosestToWithDistance q

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance. Returns both the point and the
  -- distance realized by this point.
  pointClosestToWithDistance     :: ( Num (NumType g)
                                    , Point_ point    (Dimension g) (NumType g)
                                    , Point_ outPoint (Dimension g) (NumType g)
                                    )
                                 => point -> g
                                 -> (outPoint, NumType g)
  pointClosestToWithDistance q g = let q' = pointFromPoint q
                                       p  = pointClosestTo q' g
                                   in (p, squaredEuclideanDist p q')
