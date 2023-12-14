{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- d-dimensional Boxes
--
--------------------------------------------------------------------------------
module HGeometry.Box.Intersection
  (
  ) where

import Control.Lens
import Data.Maybe (isJust)
import Data.Semialign
import HGeometry.Box.Class
import HGeometry.Box.Internal
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

type instance Intersection (Box point) (Box point) =
  Maybe (Box (Point (Dimension point) (NumType point)))

-- TODO, maybe make this more precise:

-- data instance IntersectionOf (Box point) (Box point) =
--     Box_x_Box_Point point
--   | Box_x_Box_Segment (ClosedLineSegment point)
--   | Box_x_Box_Box (Box point)

instance (Ord r, Num r, Point_ point d r
         , HasComponents (Vector d (IntersectionOf (ClosedInterval r) (ClosedInterval r)))
                         (Vector d (ClosedInterval r))
         , HasComponents (Vector d (ClosedInterval r)) (Vector d r)
         , Has_ Vector_ d (ClosedInterval r)
         , Has_ Additive_ d r
         , Traversable (Vector d), Applicative (Vector d), Zip (Vector d)
         ) => Box point `HasIntersectionWith` Box point where
  a `intersects` b = isJust $ a `intersect` b

instance  ( Point_ point d r, Ord r, Num r
          , HasComponents (Vector d (IntersectionOf (ClosedInterval r) (ClosedInterval r)))
                          (Vector d (ClosedInterval r))
          , HasComponents (Vector d (ClosedInterval r)) (Vector d r)
          , Has_ Vector_ d (ClosedInterval r)
          , Has_ Additive_ d r
          , Traversable (Vector d), Applicative (Vector d), Zip (Vector d)
          ) => Box point `IsIntersectableWith` Box point where
  bx `intersect` bx' = fmap fromExtent' . sequence $ liftA2 intersect (extent bx) (extent bx')
    where
      fromExtent' = fromExtent . over (components @_ @(Vector d (ClosedInterval r))) f
      f = \case
        ClosedInterval_x_ClosedInterval_Point x     -> ClosedInterval x x
        ClosedInterval_x_ClosedInterval_Contained i -> i
        ClosedInterval_x_ClosedInterval_Partial i   -> i
  {-# INLINE intersect #-}
