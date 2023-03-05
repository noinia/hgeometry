{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- d-dimensional Boxes
--
--------------------------------------------------------------------------------
module HGeometry.Box
  ( module HGeometry.Box.Class
  , module HGeometry.Box.Internal
  , IsBoxable(..)
  , LineBoxIntersection(..)
  ) where

import Control.Lens
import HGeometry.Box.Boxable
import HGeometry.Box.Class
import HGeometry.Box.Internal
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Line.LineEQ
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Data type representing the intersection of a Box and a line
data LineBoxIntersection d r = Line_x_Box_Point (Point d r)
                             | Line_x_Box_Segment (ClosedLineSegment (Point d r))

deriving instance (Show (Point d r), Show (ClosedLineSegment (Point d r))) =>
                  Show (LineBoxIntersection d r)
deriving instance (Eq (Point d r), Eq (ClosedLineSegment (Point d r))) =>
                  Eq (LineBoxIntersection d r)

type instance Intersection (LineEQ r) (Rectangle point) = Maybe (LineBoxIntersection 2 r)

instance (Num r, Ord r
         , Point_ point 2 r
         ) =>  HasIntersectionWith (LineEQ r) (Rectangle point) where
  l@(LineEQ a b) `intersects` (Rectangle p q) = case a `compare` 0 of
      LT -> ly >= p^.yCoord && ry <= q^.yCoord
      EQ -> Point1 b `intersects` ClosedInterval (p^.yCoord) (q^.yCoord)
      GT -> ly <= q^.yCoord && ry >= p^.yCoord
    where
      ly = evalAt' (p^.xCoord) l
      ry = evalAt' (q^.xCoord) l
  {-# INLINE intersects #-}

instance (Fractional r, Ord r
         , Point_ point 2 r
         ) =>  IsIntersectableWith (LineEQ r) (Rectangle point) where
  l@(LineEQ a b) `intersect` (Rectangle p q) = case a `compare` 0 of
      LT -> case ly `compare` (p^.yCoord) of
              LT -> Nothing
              EQ -> Just . Line_x_Box_Point $ Point2 (p^.xCoord) (p^.yCoord)
              GT -> case ry `compare` (q^.yCoord) of
                      LT -> let s = if p^.xCoord <= tx then Point2 tx (p^.yCoord)
                                                       else Point2 (p^.xCoord) ly
                                t = if bx <= q^.xCoord then Point2 bx (q^.yCoord)
                                                       else Point2 (q^.xCoord) ry
                            in Just . Line_x_Box_Segment $ ClosedLineSegment s t
                      EQ -> Just . Line_x_Box_Point $ Point2 (q^.xCoord) (q^.yCoord)
                      GT -> Nothing
      EQ | inRange   -> Just . Line_x_Box_Segment
                      $ ClosedLineSegment (Point2 (p^.xCoord) b) (Point2 (q^.xCoord) b)
         | otherwise -> Nothing
      GT -> case ly `compare` (q^.yCoord) of
              LT -> case ry `compare` (p^.yCoord) of
                      LT -> Nothing
                      EQ -> Just . Line_x_Box_Point $ Point2 (q^.xCoord) (p^.yCoord)
                      GT -> let
                                s = if p^.xCoord <= bx then Point2 bx (p^.yCoord)
                                                       else Point2 (p^.xCoord) ly
                                t = if tx <= q^.xCoord then Point2 tx (q^.yCoord)
                                                       else Point2 (q^.xCoord) ry
                            in Just . Line_x_Box_Segment $ ClosedLineSegment s t
              EQ -> Just . Line_x_Box_Point $ Point2 (p^.xCoord) (q^.yCoord)
              GT -> Nothing
    where
      ly = evalAt' (p^.xCoord) l
      ry = evalAt' (q^.xCoord) l

      bx = horX (p^.yCoord)
      tx = horX (q^.yCoord)

      inRange = Point1 b `intersects` ClosedInterval (p^.yCoord) (q^.yCoord)

      -- x-coordinate of the intersection with a horizontal line at height h
      horX h = (h-b) / a
  {-# INLINE intersect #-}


myRect :: Rectangle (Point 2 Rational)
myRect = Rectangle origin (Point2 10 20.0)
