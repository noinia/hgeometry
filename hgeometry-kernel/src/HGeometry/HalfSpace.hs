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
  , convertBoundingLineOf
  ) where

import HGeometry.HalfSpace.Class
import HGeometry.HalfSpace.Type
import HGeometry.HalfSpace.Intersection
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Line.PointAndVector
import HGeometry.Line.Class
import HGeometry.Intersection
import HGeometry.Sign
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


--------------------------------------------------------------------------------
-- * Specific functions for 2D Halfspaces, aka HalfPlanes

-- | Convert a halfplane bounded by an oriented line a halfplane
-- bounded by some more general line type.
convertBoundingLineOf   :: (Line2_ line r, Ord r, Num r) => HalfPlaneF (LinePV 2 r) -> HalfPlaneF line
convertBoundingLineOf h =
  let h' = h&boundingHyperPlaneLens %~ \(LinePV p v) -> fromPointAndVec p v
  in h'&halfSpaceSign %~ \s -> if pointInteriorTo h `intersects` h'
                               then s else flipSign s
