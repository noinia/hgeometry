{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.LineSegment.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class representing line segments
--
--------------------------------------------------------------------------------
module Geometry.LineSegment.Class
  ( LineSegment_(..), pattern LineSegment_

  , interpolate
  , OnSegment(..)
  ) where

import Control.Lens
import Data.Ord (comparing)
import Geometry.Interval.Class
import Geometry.Point.Class
import Geometry.Vector

--------------------------------------------------------------------------------

class OnSegment lineSegment d point r where
  -- | Test if a point lies on a line segment.
  --
  -- As a user, you should typically just use 'intersects' instead.
  onSegment :: (Ord r, Point_ point' d r) => point' d r -> lineSegment d point r -> Bool


class ( HasStart (lineSegment d point r) (point d r)
      , HasEnd   (lineSegment d point r) (point d r)
      , Point_ point d r
      ) => LineSegment_ lineSegment d point r where
  {-# MINIMAL uncheckedLineSegment #-}

  -- | Create a segment
  --
  -- pre: the points are disjoint
  uncheckedLineSegment :: point d r -> point d r -> lineSegment d point r

  -- | smart constructor that creates a valid segment, i.e. it
  -- validates that the endpoints are disjoint.
  mkLineSegment                  :: Eq r => point d r -> point d r -> Maybe (lineSegment d point r)
  mkLineSegment s t
    | s^.asVector /= t^.asVector = Just $ uncheckedLineSegment s t
    | otherwise                  = Nothing


-- | Constructs a line segment from the start and end point
pattern LineSegment_ :: forall lineSegment d point r. LineSegment_ lineSegment d point r
                     => point d r -> point d r
                     -> lineSegment d point r
pattern LineSegment_ s t <- (startAndEnd -> (s,t))
  where
    LineSegment_ s t = uncheckedLineSegment s t

--------------------------------------------------------------------------------

-- | Linearly interpolate the two endpoints with a value in the range [0,1]
--
-- >>> interpolate 0.5 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 5.0 5.0
-- >>> interpolate 0.1 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 1.0 1.0
-- >>> interpolate 0 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 0.0 0.0
-- >>> interpolate 1 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 10.0 10.0
interpolate       :: forall lineSegment d point r
                     . (Fractional r, LineSegment_ lineSegment d point r)
                  => r -> lineSegment d point r -> point d r
interpolate lam (LineSegment_ s t) =
  fromVector $ (s^.asVector ^* (1-lam)) ^+^ (t^.asVector ^* lam)


--------------------------------------------------------------------------------
-- * Convenience functions for working with 2-dimensional line segments

-- | Given a y-coordinate, compare the segments based on the
-- x-coordinate of the intersection with the horizontal line through y
ordAtY   :: (Fractional r, Ord r, LineSegment_ lineSegment 2 point r)
         => r
         -> lineSegment 2 point r -> lineSegment 2 point r -> Ordering
ordAtY y = comparing (xCoordAt y)

-- | Given an x-coordinate, compare the segments based on the
-- y-coordinate of the intersection with the horizontal line through y
ordAtX   :: (Fractional r, Ord r, LineSegment_ lineSegment 2 point r)
         => r
         -> lineSegment 2 point r -> lineSegment 2 point r -> Ordering
ordAtX x = comparing (yCoordAt x)

-- | Given a y coord and a line segment that intersects the horizontal line
-- through y, compute the x-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
xCoordAt :: (Fractional r, Ord r, LineSegment_ lineSegment 2 point r)
         => r -> lineSegment 2 point r -> r
xCoordAt y (LineSegment_ (Point2_ px py) (Point2_ qx qy))
      | py == qy     = px `max` qx  -- s is horizontal, and since it by the
                                    -- precondition it intersects the sweep
                                    -- line, we return the x-coord of the
                                    -- rightmost endpoint.
      | otherwise    = px + alpha * (qx - px)
  where
    alpha = (y - py) / (qy - py)

-- | Given an x-coordinate and a line segment that intersects the vertical line
-- through x, compute the y-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
yCoordAt :: (Fractional r, Ord r, LineSegment_ lineSegment 2 point r)
         => r -> lineSegment 2 point r -> r
yCoordAt x (LineSegment_ (Point2_ px py) (Point2_ qx qy))
    | px == qx  = py `max` qy -- s is vertical, since by the precondition it
                              -- intersects we return the y-coord of the topmost
                              -- endpoint.
    | otherwise = py + alpha * (qy - py)
  where
    alpha = (x - px) / (qx - px)

--------------------------------------------------------------------------------
