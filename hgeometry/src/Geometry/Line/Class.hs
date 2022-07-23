{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Line.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional lines.
--
--------------------------------------------------------------------------------
module Geometry.Line.Class
  ( Line_(..), pattern Line_
  , lineThrough
  , verticalLine, horizontalLine
  , perpendicularTo, isPerpendicularTo

  , isParallelTo
  , onLine

  -- , isIdenticalTo,
  , pointAt
  , toOffset, toOffset'
  ) where

import Control.Lens
import Geometry.Point.Class
import Geometry.Point.Internal
import Geometry.Vector
import Geometry.Properties

--------------------------------------------------------------------------------

class ( NumType (line d r) ~ r
      , Dimension (line d r) ~ d
      ) => Line_ line d r where
  {-# MINIMAL mkLine, anchorPoint, direction#-}
  -- | Constructs the line through the given point with the given direction.
  mkLine :: Point_ point d r => point d r -> Vector d r -> line d r
  -- | Lens to access a point on the line
  anchorPoint :: Lens' (line d r) (Point d r)
  -- | The direction of the line
  direction :: Lens' (line d r) (Vector d r)


-- | destruct a line into an anchorpoint and a vector.
pointAndVec   :: Line_ line d r => line d r -> (Point d r, Vector d r)
pointAndVec l = (l^.anchorPoint, l^.direction)

-- | Pattern to access a line.
pattern Line_ :: (Line_ line d r, Arity d) => Point d r -> Vector d r -> line d r
pattern Line_ p v <- (pointAndVec -> (p,v))
  where
    Line_ p v = mkLine p v

-- | Test if the two lines are parallel.
--
-- >>> lineThrough origin (Point2 1 0) `isParallelTo` lineThrough (Point2 1 1) (Point2 2 1)
-- True
-- >>> lineThrough origin (Point2 1 0) `isParallelTo` lineThrough (Point2 1 1) (Point2 2 2)
-- False
isParallelTo :: (Eq r, Fractional r, Arity d, Line_ line d r) => line d r -> line d r -> Bool
l `isParallelTo` m = (l^.direction) `isScalarMultipleOf` (m^.direction)

-- | Test if point p lies on line l
--
-- >>> (origin :: Point 2 Rational) `onLine` lineThrough origin (Point2 1 0)
-- True
-- >>> Point2 10 10 `onLine` lineThrough origin (Point2 2 2)
-- True
-- >>> Point2 10 5 `onLine` lineThrough origin (Point2 2 2)
-- False
onLine :: (Eq r, Fractional r, Point_ point d r, Line_ line d r) => point d r -> line d r -> Bool
(fromGenericPoint -> p) `onLine` (pointAndVec -> (q,v)) =
  p == q || (p .-. q) `isScalarMultipleOf` v


-- | constructs a line may be constructed from two points.
lineThrough     :: forall line point d r. (Num r, Point_ point d r, Line_ line d r) => point d r -> point d r -> line d r
lineThrough p q = mkLine p (q .-. p)

-- | Vertical line with a given X-coordinate.
verticalLine   :: forall line r. (Num r, Line_ line 2 r) => r -> line 2 r
verticalLine x = mkLine (Point2 x 0) (Vector2 0 1)

-- | Horizontal line with a given Y-coordinate.
horizontalLine   :: forall line r. (Num r, Line_ line 2 r) => r -> line 2 r
horizontalLine y = mkLine (Point2 0 y) (Vector2 1 0)

-- | Given a line l with anchor point p and vector v, get the line
-- perpendicular to l that also goes through p. The resulting line m is
-- oriented such that v points into the left halfplane of m.
--
-- >>> perpendicularTo $ Line (Point2 3 4) (Vector2 (-1) 2)
-- Line (Point2 3 4) (Vector2 (-2) (-1))
perpendicularTo   :: (Num r, Line_ line 2 r) => line 2 r -> line 2 r
perpendicularTo l = l&direction %~ \(Vector2 vx vy) -> Vector2 (-vy) vx

-- | Test if a vector is perpendicular to the line.
isPerpendicularTo :: (Num r, Eq r, Line_ line 2 r) => Vector 2 r -> line 2 r -> Bool
v `isPerpendicularTo` l = v `dot` (l^.direction) == 0

-- | Get the point at the given position along line, where 0 corresponds to the
-- anchorPoint of the line, and 1 to the point anchorPoint .+^ directionVector
pointAt               :: (Num r, Arity d, Line_ line d r) => r -> line d r -> Point d r
pointAt a l = l^.anchorPoint .+^ (a *^ (l^.direction))

-- | Given point p and a line (Line q v), Get the scalar lambda s.t.
-- p = q + lambda v. If p does not lie on the line this returns a Nothing.
toOffset                          :: (Eq r, Fractional r, Line_ line d r, Point_ point d r)
                                  => point d r -> line d r -> Maybe r
toOffset p (pointAndVec -> (q,v)) = scalarMultiple (fromGenericPoint  p .-. q) v

-- | Given point p near a line (Line q v), get the scalar lambda s.t.
-- the distance between 'p' and 'q + lambda v' is minimized.
--
-- >>> toOffset' (Point2 1 1) (lineThrough origin $ Point2 10 10)
-- 0.1
--
-- >>> toOffset' (Point2 5 5) (lineThrough origin $ Point2 10 10)
-- 0.5
--
-- The point (6,4) is not on the line but we can still point closest to it.
-- >>> toOffset' (Point2 6 4) (lineThrough origin $ Point2 10 10)
-- 0.5
toOffset'             :: (Eq r, Fractional r, Line_ line d r, Point_ point d r)
                      => point d r -> line d r -> r
toOffset' (fromGenericPoint -> p) (pointAndVec -> (q,v)) = dot (p .-. q) v / quadrance v
-- toOffset' p = fromJust' . toOffset p
--   where
--     fromJust' (Just x) = x
--     fromJust' _        = error "toOffset: Nothing"
