{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Point.EuclideanDistance
  ( cmpByDistanceTo
  , squaredEuclideanDist, euclideanDist
  -- , HasSquaredEuclideanDistance(..)
  ) where

import           Control.Lens
import           D
import           Data.Ord (comparing)
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point.Class
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           Point
import           R


--------------------------------------------------------------------------------
-- * Distances

-- | Squared Euclidean distance between two points
squaredEuclideanDist     :: Num R => Point D R -> Point D R -> R
squaredEuclideanDist p q = quadrance $ p .-. q

-- | Euclidean distance between two points
euclideanDist     :: Radical.Radical R => Point D R -> Point D R -> R
euclideanDist p q = Radical.sqrt $ squaredEuclideanDist p q

-- | Compare two points by their distance to the first argument
cmpByDistanceTo   :: (Ord R, Num R)
                  => Point D R -> Point D R -> Point D R -> Ordering
cmpByDistanceTo c = comparing (squaredEuclideanDist c)

--------------------------------------------------------------------------------

{-

-- | Types for which we can compute the squared Euclidean distance.
class HasSquaredEuclideanDistance g where
  {-# MINIMAL pointClosestToWithDistance | pointClosestTo #-}
  -- | Given a point q and a geometry g, the squared Euclidean distance between q and g.
  squaredEuclideanDistTo   :: ( r ~ NumType g, d ~ Dimension g, Num r
                              , Point_ point d r
                              )
                           => point -> g -> NumType g
  squaredEuclideanDistTo q = snd . pointClosestToWithDistance q
  {-# INLINE squaredEuclideanDistTo #-}

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance.
  pointClosestTo   :: ( r ~ NumType g, d ~ Dimension g, Num r
                      , Point_ point d r
                      )
                   => point -> g -> Point (Dimension g) (NumType g)
  pointClosestTo q = fst . pointClosestToWithDistance q
  {-# INLINE pointClosestTo #-}

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance. Returns both the point and the
  -- distance realized by this point.
  pointClosestToWithDistance     :: ( r ~ NumType g
                                    , d ~ Dimension g
                                    , Num r
                                    , Point_ point d r
                                    )
                                 => point -> g
                                 -> (Point d r, r)
  pointClosestToWithDistance q g = let q' = Point $ q^.vector
                                       p  = pointClosestTo q' g
                                   in (p, squaredEuclideanDist p q')
  {-# INLINE pointClosestToWithDistance #-}

instance ( Vector_ v
         , IxValue v ~ NumType v
         ) => HasSquaredEuclideanDistance (PointF v) where
  pointClosestTo _ p = Point $ p^.vector
  {-# INLINE pointClosestTo #-}

-}
