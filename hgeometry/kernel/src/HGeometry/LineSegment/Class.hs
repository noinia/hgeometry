{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class representing line segments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Class
  ( LineSegment_
  , pattern LineSegment_
  , ConstructableLineSegment_(..)
  , ClosedLineSegment_
  , OpenLineSegment_

  , interpolate
  , HasOnSegment(..)

  , HasStart(..), HasEnd(..)
  , HasStartPoint(..), HasEndPoint(..)
  , StartPointOf, EndPointOf

  , ordAtY, ordAtX
  , xCoordAt, yCoordAt

  , orientLR, orientBT
  ) where

import Control.Lens
import Data.Default.Class
import Data.Type.Ord
import HGeometry.Ext
import HGeometry.Interval.Class
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.LineSegment
-- >>> import HGeometry.Point

-- not sure why I made this a separate class again...

-- | Class that expresses that we can test if the segment contains a point
class HasOnSegment lineSegment d | lineSegment -> d where
  -- | Test if a point lies on a line segment.
  --
  -- As a user, you should typically just use 'intersects' instead.
  onSegment :: ( Ord r, Point_ point d r
               , r ~ NumType lineSegment, d ~ Dimension lineSegment
               ) => point -> lineSegment -> Bool

-- | A class representing line segments
class ( IntervalLike_ lineSegment point
      , Point_ point (Dimension lineSegment) (NumType lineSegment)
      ) => LineSegment_ lineSegment point | lineSegment -> point where
  {-# MINIMAL  #-}

-- | A class representing line segments
class ( LineSegment_ lineSegment point
      ) => ConstructableLineSegment_ lineSegment point where
  {-# MINIMAL uncheckedLineSegment #-}

  -- | Create a segment
  --
  -- pre: the points are disjoint
  uncheckedLineSegment     :: point -> point -> lineSegment
  -- uncheckedLineSegment s t = mkInterval (mkEndPoint s) (mkEndPoint t)

  -- | smart constructor that creates a valid segment, i.e. it
  -- validates that the endpoints are disjoint.
  mkLineSegment              :: Eq (Vector (Dimension point) (NumType point))
                             => point -> point -> Maybe lineSegment
  mkLineSegment s t
    | s^.vector /= t^.vector = Just $ uncheckedLineSegment s t
    | otherwise              = Nothing




-- | A class representing Closed Linesegments
class ( LineSegment_ lineSegment point
      , StartPointOf lineSegment ~ EndPoint Closed point
      , EndPointOf   lineSegment ~ EndPoint Closed point
      ) => ClosedLineSegment_ lineSegment point where

-- | A Class representing Open ended linesegments
class ( LineSegment_ lineSegment point
      , StartPointOf lineSegment ~ EndPoint Open point
      , EndPointOf   lineSegment ~ EndPoint Open point
      ) => OpenLineSegment_ lineSegment point where


-- | Deconstructs a line segment from the start and end point
pattern LineSegment_     :: forall lineSegment point. LineSegment_ lineSegment point
                         => point -> point -> lineSegment
pattern LineSegment_ s t <- (startAndEnd -> (s,t))
{-# COMPLETE LineSegment_ #-}

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
                     . ( Fractional r, LineSegment_ lineSegment point
                       , ConstructablePoint_ point d r
                       )
                  => r -> lineSegment -> point
interpolate lam (LineSegment_ s t) =
  fromVector $ (s^.vector ^* (1-lam)) ^+^ (t^.vector ^* lam)


--------------------------------------------------------------------------------
-- * Convenience functions for working with 2-dimensional line segments

-- | Given a y-coordinate, compare the segments based on the
-- x-coordinate of the intersection with the horizontal line through y
ordAtY   :: (Num r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
         => r
         -> lineSegment -> lineSegment -> Ordering
ordAtY y seg1 seg2 = ordAtX (-y) (flipPlane seg1) (flipPlane seg2)
  where
    rot90 (Vector2 x' y') = Vector2 (-y') x'
    flipPlane seg = seg&start.vector %~ rot90
                       &end.vector   %~ rot90

-- | Given a y coord and a line segment that intersects the horizontal line
-- through y, compute the x-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
xCoordAt :: (Fractional r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
         => r -> lineSegment -> r
xCoordAt y (LineSegment_ (Point2_ px py) (Point2_ qx qy))
      | py == qy     = px `max` qx  -- s is horizontal, and since it by the
                                    -- precondition it intersects the sweep
                                    -- line, we return the x-coord of the
                                    -- rightmost endpoint.
      | otherwise    = px + alpha * (qx - px)
  where
    alpha = (y - py) / (qy - py)


-- | Given an x-coordinate, compare the segments based on the
-- y-coordinate of the intersection with the horizontal line through y
ordAtX   :: ( Num r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
         => r
         -> lineSegment -> lineSegment -> Ordering
ordAtX x (orientLR -> LineSegment_ (Point2_ px py) (Point2_ qx qy))
         (orientLR -> LineSegment_ (Point2_ ax ay) (Point2_ bx by)) =
    case (pqVertical,abVertical) of
      (True,True)   -> pqTop           `compare` abTop
      (True,False)  -> (pqTop*(bx-ax)) `compare` abTerm
      (False,True)  -> pqTerm          `compare` (abTop*(qx-px))
      (False,False) -> term1           `compare` term2
  where
    -- For ease of argument we orient the segment from left to right.
    --
    -- The main idea is to essentially take te yCoordAt implementation, and multiply out te
    -- divisor in te alpha term. More specifically:
    --
    -- for segment pq, the intersection y-coordinate is:
    --
    -- y = py + ( (x-px) / (qx-px) )*(qy-py)
    --
    -- similarly for the ab segment we have y' = ay + ( (x-ax) / (bx-ax) )*(by-ay)
    --
    -- and we wish to : y `compare` y'
    -- hence we multiply both sides by (bx-ax)*(qx-px) to get rid of the factions.
    --
    pqVertical = px == qx
    abVertical = ax == bx
    pqTop = py `max` qy
    abTop = ay `max` by

    -- the py + alpha*(qy-py) term multiplied by (qx-px) so that there are no
    -- more fractions
    pqTerm = py*(qx-px) + (x-px)*(qy-py)
    abTerm = ay*(bx-ax) + (x-ax)*(by-ay) --

    term1 = (bx-ax) * pqTerm
    term2 = (qx-px) * abTerm

-- | Orient the segment from left to right
orientLR     :: (LineSegment_ lineSegment point, Point_ point d r, 1 <= d, Ord r)
             => lineSegment -> lineSegment
orientLR seg
  | seg^.start.xCoord <= seg^.end.xCoord = seg
  | otherwise                            = seg&start .~ (seg^.end)
                                              &end   .~ (seg^.start)


-- | Orient the segment from bottom to top
orientBT     :: (LineSegment_ lineSegment point, Point_ point d r, 2 <= d, Ord r)
             => lineSegment -> lineSegment
orientBT seg
  | seg^.start.yCoord <= seg^.end.yCoord = seg
  | otherwise                            = seg&start .~ (seg^.end)
                                              &end   .~ (seg^.start)



-- | Given an x-coordinate and a line segment that intersects the vertical line
-- through x, compute the y-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
yCoordAt :: ( Fractional r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
         => r -> lineSegment -> r
yCoordAt x (LineSegment_ (Point2_ px py) (Point2_ qx qy))
    | px == qx  = py `max` qy -- s is vertical, since by the precondition it
                              -- intersects we return the y-coord of the topmost
                              -- endpoint.
    | otherwise = py + alpha * (qy - py)
  where
    alpha = (x - px) / (qx - px)


--------------------------------------------------------------------------------

instance ( LineSegment_ segment point
         ) => LineSegment_ (segment :+ extra) point where

instance ( ConstructableLineSegment_ segment point
         , Default extra
         ) => ConstructableLineSegment_ (segment :+ extra) point where
  uncheckedLineSegment p q = uncheckedLineSegment p q :+ def

instance ( ClosedLineSegment_ segment point
         ) => ClosedLineSegment_ (segment :+ extra) point where

instance ( OpenLineSegment_ segment point
         ) => OpenLineSegment_ (segment :+ extra) point where

instance HasOnSegment lineSegment d =>  HasOnSegment (lineSegment :+ extra) d where
  onSegment q (s :+ _) = q `onSegment` s
