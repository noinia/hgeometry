{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.EuclideanDistance
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Points that have to do with Euclidean distances between
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module HGeometry.Point.EuclideanDistance
  ( cmpByDistanceTo
  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)
  ) where

import           Control.Lens
import           Data.Ord (comparing)
import           HGeometry.Ext
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point.Class
import           HGeometry.Point.Type
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * Distances

-- | Squared Euclidean distance between two points
squaredEuclideanDist     :: (Num r, Point_ point d r, Metric_ (Vector d r) d r)
                         => point -> point -> r
squaredEuclideanDist p q = quadrance $ p .-. q
{-# INLINE squaredEuclideanDist #-}

-- | Euclidean distance between two points
euclideanDist     :: (Radical.Radical r, Point_ point d r, Metric_ (Vector d r) d r)
                  => point -> point -> r
euclideanDist p q = Radical.sqrt $ squaredEuclideanDist p q
{-# INLINE euclideanDist #-}

-- | Compare two points by their distance to the first argument
cmpByDistanceTo   :: (Ord r, Num r, Point_ point d r, Metric_ (Vector d r) d r)
                  => point -> point -> point -> Ordering
cmpByDistanceTo c = comparing (squaredEuclideanDist c)
{-# INLINE cmpByDistanceTo #-}

--------------------------------------------------------------------------------

-- | Types for which we can compute the squared Euclidean distance.
class Metric_ (Vector (Dimension g) (NumType g)) (Dimension g) (NumType g)
      => HasSquaredEuclideanDistance g where
  {-# MINIMAL pointClosestToWithDistance | pointClosestTo #-}
  -- | Given a point q and a geometry g, the squared Euclidean distance between q and g.
  squaredEuclideanDistTo   :: ( r ~ NumType g
                              , d ~ Dimension g
                              , Num r
                              , Point_ point d r
                              )
                           => point -> g -> r
  squaredEuclideanDistTo q = snd . pointClosestToWithDistance q
  {-# INLINE squaredEuclideanDistTo #-}

  -- | Given q and g, computes the point p in g closest to q according
  -- to the Squared Euclidean distance.
  pointClosestTo   :: ( r ~ NumType g
                      , d ~ Dimension g
                      , Num r
                      , Point_ point d r
                      )
                   => point -> g -> Point d r
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

instance ( Vector_ v            d r
         , Metric_ (Vector d r) d r
         ) => HasSquaredEuclideanDistance (PointF v) where
  pointClosestTo _ p = Point $ p^.vector
  {-# INLINE pointClosestTo #-}


instance HasSquaredEuclideanDistance p => HasSquaredEuclideanDistance (p :+ extra) where
  pointClosestTo q = pointClosestTo q . view core
  {-# INLINE pointClosestTo #-}
