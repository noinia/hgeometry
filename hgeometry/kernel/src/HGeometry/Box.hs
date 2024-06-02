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
  , module HGeometry.Box.Corners
  , module HGeometry.Box.Sides
  , IsBoxable(..)
  , LineBoxIntersection(..)
  , HalfLineBoxIntersection(..)
  ) where

import Control.Lens
import Data.Foldable1
import Data.Semialign
import Data.Semigroup (All(..))
import GHC.Generics (Generic)
import HGeometry.Boundary
import HGeometry.Box.Boxable
import HGeometry.Box.Class
import HGeometry.Box.Corners
import HGeometry.Box.Internal
import HGeometry.Box.Intersection ()
import HGeometry.Box.Sides
import HGeometry.Direction
import HGeometry.HalfLine
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Line.General
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.LineSegment.Intersection
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector
import Prelude hiding (zipWith)

-- import Debug.Trace

--------------------------------------------------------------------------------
-- $setup
-- >>> myRect = Rectangle origin (Point2 10 20.0) :: Rectangle (Point 2 Rational)
--
--

--------------------------------------------------------------------------------

instance ( Point_ point d r, Num r, Ord r
         , Foldable1 (Vector d), Zip (Vector d)
         ) => HasIntersectionWith (Point d r) (Box point) where
  q `intersects` box = getAll . fold1 $
    zipWith (\x i -> All $ Point1 x `intersects` i) (q^.vector) (extent box)
  {-# INLINE intersects #-}

type instance Intersection (Point d r) (Box point) = Maybe (Point d r)

instance ( Point_ point d r, Num r, Ord r
         , Foldable1 (Vector d), Zip (Vector d)
         ) => IsIntersectableWith (Point d r) (Box point) where
  q `intersect` box
    | q `intersects` box = Just q
    | otherwise          = Nothing
  {-# INLINE intersect #-}


--------------------------------------------------------------------------------
-- * Intersection with a line

-- | Data type representing the intersection of a Box and a line
data LineBoxIntersection d r = Line_x_Box_Point (Point d r)
                             | Line_x_Box_LineSegment (ClosedLineSegment (Point d r))

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
                      LT -> let s = if p^.xCoord <= tx then Point2 tx (q^.yCoord)
                                                       else Point2 (p^.xCoord) ly
                                t = if bx <= q^.xCoord then Point2 bx (p^.yCoord)
                                                       else Point2 (q^.xCoord) ry
                            in Just . Line_x_Box_LineSegment $ ClosedLineSegment s t
                      EQ -> Just . Line_x_Box_Point $ Point2 (q^.xCoord) (q^.yCoord)
                      GT -> Nothing
      EQ | inRange   -> Just . Line_x_Box_LineSegment
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
                            in Just . Line_x_Box_LineSegment $ ClosedLineSegment s t
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

----------------------------------------
-- with general line

type instance Intersection (VerticalOrLineEQ r) (Rectangle point) =
  Maybe (LineBoxIntersection 2 r)

instance (Num r, Ord r
         , Point_ point 2 r
         ) =>  HasIntersectionWith (VerticalOrLineEQ r) (Rectangle point) where
  l `intersects` rect@(Rectangle p q) = case l of
    VerticalLineThrough x -> (p^.xCoord) <= x && x <= (q^.xCoord)
    NonVertical l'        -> l' `intersects` rect
  {-# INLINE intersects #-}

instance (Fractional r, Ord r
         , Point_ point 2 r
         ) =>  IsIntersectableWith (VerticalOrLineEQ r) (Rectangle point) where
  l `intersect` rect@(Rectangle p q) = case l of
    VerticalLineThrough x
      | (p^.xCoord) <= x && x <= (q^.xCoord) -> Just . Line_x_Box_LineSegment
                                              $ ClosedLineSegment (Point2 x $ p^.yCoord)
                                                                  (Point2 x $ q^.yCoord)
      | otherwise                            -> Nothing
    NonVertical l'        -> l' `intersect` rect
  {-# INLINE intersect #-}





instance (Point_ point d r, IsTransformable point) => IsTransformable (Box point) where
  -- ^ this instance is slighly misleading, as for arbitrary affine
  -- transformations (e.g. rotations) the result may no longer be an
  -- axis aligned box. So use with care!
  transformBy t = over allPoints (transformBy t)


type instance Intersection (LinePV 2 r) (Rectangle point) = Maybe (LineBoxIntersection 2 r)

instance ( Num r, Ord r
         , Point_ point 2 r
         ) =>  HasIntersectionWith (LinePV 2 r) (Rectangle point) where
  l `intersects` (corners -> Corners tl tr br bl) =
      onOppositeSides tl br || onOppositeSides tr bl
    where
      onOppositeSides p q = onSideTest p l /= onSideTest q l
  {-# INLINE intersects #-}

instance ( Fractional r, Ord r
         , Point_ point 2 r
         ) =>  IsIntersectableWith (LinePV 2 r) (Rectangle point) where
  l@(LinePV p _) `intersect` r = case toLinearFunction l of
      Nothing
        | (p^.xCoord) `stabsInterval` xRange -> Just . Line_x_Box_LineSegment
                                              $ ClosedLineSegment (Point2 (p^.xCoord) minY)
                                                                  (Point2 (p^.xCoord) maxY)
        | otherwise                          -> Nothing
      Just l'                                -> l' `intersect` r
    where
      Vector2 xRange (ClosedInterval minY maxY) = extent r
  {-# INLINE intersect #-}

--------------------------------------------------------------------------------
-- Box x Box intersection






--------------------------------------------------------------------------------
-- * Intersection with a linesegment

-- | Figure out where a query point is with respect to a rectangle
inBox          :: ( Point_ point 2 r, Point_ queryPoint 2 r, Rectangle_ rectangle point
                  , Ord r, Num r
                  ) => queryPoint -> rectangle -> PointLocationResultWith CardinalDirection
q `inBox` rect = case compareIntervalExact (q^.xCoord) xRange of
    Before   -> StrictlyOutside
    OnStart  -> case compareIntervalExact (q^.yCoord) yRange of
                  Before   -> StrictlyOutside
                  OnStart  -> OnBoundaryEdge South -- on both south and west even
                  Interior -> OnBoundaryEdge West
                  OnEnd    -> OnBoundaryEdge North -- on both north and west
                  After    -> StrictlyOutside
    Interior -> case compareIntervalExact (q^.yCoord) yRange of
                  Before   -> StrictlyOutside
                  OnStart  -> OnBoundaryEdge South
                  Interior -> StrictlyInside
                  OnEnd    -> OnBoundaryEdge North
                  After    -> StrictlyOutside
    OnEnd    -> case compareIntervalExact (q^.yCoord) yRange of
                  Before   -> StrictlyOutside
                  OnStart  -> OnBoundaryEdge East -- on both south and east even
                  Interior -> OnBoundaryEdge East
                  OnEnd    -> OnBoundaryEdge North -- on both north and east
                  After    -> StrictlyOutside
    After    -> StrictlyOutside
  where
    Vector2 xRange yRange = extent rect


instance ( Ord r, Num r, Point_ point 2 r, Point_ point' 2 r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , HasIntersectionWith  (LineSegment endPoint point) (ClosedLineSegment point')
         ) => HasIntersectionWith (LineSegment endPoint point) (Rectangle point') where
  seg `intersects` rect = intersects'
    where
      intersects' = case (seg^.start) `inBox` rect of
        StrictlyInside   -> True
        OnBoundaryEdge _ -> isClosed (seg^.startPoint)
                            || case (seg^.end) `inBox` rect of
                                 StrictlyInside   -> True
                                 OnBoundaryEdge _ -> True
                                 StrictlyOutside  -> intersectsBoundary
        StrictlyOutside  -> case (seg^.end) `inBox` rect of
                              StrictlyInside   -> True
                              OnBoundaryEdge _ -> isClosed (seg^.endPoint) || intersectsBoundary
                              StrictlyOutside  -> intersectsBoundary
      intersectsBoundary = seg `intersects` Boundary rect
      isClosed = (== Closed) . endPointType

-- isOpenSegment     :: ( IxValue (endPoint point) ~ point
--                      , EndPoint_ (endPoint point)
--                      ) => LineSegment endPoint point -> Bool
-- isOpenSegment seg = let isOpen = (== Open) . endPointType
--                     in isOpen (seg^.startPoint) && isOpen (seg^.endPoint)

-- diagonals (corners -> Corners a b c d) = Vector2 (ClosedLineSegment a c) (ClosedLineSegment b d)


instance ( Ord r, Num r, Point_ point 2 r, Point_ point' 2 r
         -- , IxValue (endPoint point) ~ point
         -- , EndPoint_ (endPoint point)
         , HasIntersectionWith  (LineSegment endPoint point) (ClosedLineSegment point')
         ) => HasIntersectionWith (LineSegment endPoint point) (Boundary (Rectangle point')) where
  seg `intersects` (Boundary rect) = any (seg `intersects`) (sides rect)
  -- TODO: I guess we should be able to speed up this implmeentation, since the various
  -- intersection tests against the sidees are probably doing double work.


-- data LineSegmentRectangleIntersection seg point =
--     LineSegment_x_Box_Point (Point d r)
--   | LineSegment_x_Box_Segment_Contained seg
--   | LineSegment_x_Box_Segment_Partial (ClosedLineSegment (Point d r))



-- instance (Ord r, Num r, Point_ point 2 r
--          ) => LinePV 2 p `HasIntersectionWith` Boundary (Rectangle point) where
--   l `intersects` br = l `intersects` (coerce br :: Rectangle point)
--   {-# INLINE intersects #-}

-- instance (Ord r, Num r, Point_ point 2 r
--          ) => LineEQ r `HasIntersectionWith` Boundary (Rectangle point) where
--   l `intersects` br = l `intersects` (coerce br :: Rectangle point)
--   {-# INLINE intersects #-}



-- type instance Intersection (LinePV 2 r) (Rectangle point) =
--   Maybe (ClosedLineSegment (Point 2 r))

-- type instance Intersection (LineEQ r) (Rectangle point) =
--   Maybe (ClosedLineSegment (Point 2 r))


-- instance (Ord r, Fractional r, Point_ point 2 r) => LinePV 2 p `IsIntersectableWith` Rectangle point where
--   l `intersect` r = undefined

-- instance (Ord r, Fractional r, Point_ point 2 r) => LineEQ r `IsIntersectableWith` Rectangle point where
--   l `intersect` r = undefined



--------------------------------------------------------------------------------
-- * Intersection with a HalfLine


instance ( Point_ point 2 r, Ord r, Num r
         ) => HasIntersectionWith (HalfLine point) (Rectangle point) where
  hl `intersects` box =
    (hl^.start.asPoint) `intersects` box || any (hl `intersects`) (sides box)
  -- the first condition is redundant, but probably sufficiently cheap that it may
  -- actually help/be faster when the starting point lies inside the box.
  {-# INLINE intersects #-}

type instance Intersection (HalfLine point) (Rectangle point) =
  Maybe (HalfLineBoxIntersection (Point 2 (NumType point)))

data HalfLineBoxIntersection point =
    HalfLine_x_Box_Point       point
  | HalfLine_x_Box_LineSegment (ClosedLineSegment point)
  deriving (Show,Eq,Read,Generic,Functor)

instance ( Point_ point 2 r, Ord r, Fractional r
         ) => IsIntersectableWith (HalfLine point) (Rectangle point) where
  hl `intersect` box = m `intersect` box >>= \case
      Line_x_Box_Point p
        | p `onSide` perpendicularTo m == LeftSide -> Just $ HalfLine_x_Box_Point p
        | otherwise                                -> Nothing
      Line_x_Box_LineSegment seg                   -> let seg' = reorientTo (hl^.direction) seg
                                                      in case compareColinearInterval m seg' of
        Before   -> Just $ HalfLine_x_Box_LineSegment seg'
        OnStart  -> Just $ HalfLine_x_Box_LineSegment seg'
        Interior -> Just $ HalfLine_x_Box_LineSegment $ seg'&start .~ (hl^.start.asPoint)
        OnEnd    -> Just $ HalfLine_x_Box_Point (seg'^.end)
        After    -> Nothing -- no intersection
    where
      m = supportingLine hl

-- | Given some vector v, and a line segment (that suposedly either has direction v or
-- -v), flip the seg so that it has direction v.
reorientTo       :: ( Point_ point 2 r, Ord r, Num r
                    , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
                    )
                 => Vector 2 r -> LineSegment endPoint point -> LineSegment endPoint point
reorientTo v seg = case (seg^.end) `onSide` m of
                     RightSide -> flipSegment seg
                     _         -> seg
  where
    m = perpendicularTo $ LinePV (seg^.start.asPoint) v
    -- v points into the left halfplane of m
    flipSegment (LineSegment s t) = LineSegment t s
