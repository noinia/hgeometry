{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HalfSpace
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Representing halfspaces
--
--------------------------------------------------------------------------------
module HGeometry.HalfSpace
  ( module HGeometry.HalfSpace.Class
  , module HGeometry.HalfSpace.Type
  , module HGeometry.HalfSpace.Intersection
  , leftBoundingVector
  , rightBoundingVector
  ) where

import HGeometry.HalfSpace.Class
import HGeometry.HalfSpace.Type
import HGeometry.HalfSpace.Intersection
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Intersection
import Control.Lens

--------------------------------------------------------------------------------

-- | given a point a on the bounding hyperplane; compute a vector
-- pointing in the direction of the bounding line so that the
-- halfspace is to its left.
leftBoundingVector      :: ( HalfPlane_ halfPlane r
                           , Ord r, Num r
                           , HasIntersectionWith (Point 2 r) halfPlane
                           , GetDirection (BoundingHyperPlane halfPlane 2 r)
                           )
                        => Point 2 r -> halfPlane -> Vector 2 r
leftBoundingVector a h' = let l               = h'^.boundingHyperPlane
                              v@(Vector2 x y) = inLineVector l
                              w               = Vector2 (-y) x
                              -- perpendicular to v; pointing left
                          in if (a .+^ w) `intersects` h' then v else negated v
 -- it feels a bit silly we have to do this test instead of just looking
 -- at the sign of the halfplane, but alas.

-- | given a point a on the bounding hyperplane; compute a vector
-- pointing in the direction of the bounding line so that the
-- halfspace is to its right
rightBoundingVector   :: ( HalfPlane_ halfPlane r
                         , Ord r, Num r
                         , HasIntersectionWith (Point 2 r) halfPlane
                         , GetDirection (BoundingHyperPlane halfPlane 2 r)
                         )
                      => Point 2 r -> halfPlane -> Vector 2 r
rightBoundingVector p = negated . leftBoundingVector p
