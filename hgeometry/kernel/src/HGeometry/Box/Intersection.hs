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

instance (Ord r, Num r, Point_ point d r) => Box point `HasIntersectionWith` Box point where
  a `intersects` b = isJust $ a `intersect` b

instance  ( Point_ point d r, Ord r, Num r
          ) => Box point `IsIntersectableWith` Box point where
  intersect = intersectImpl

intersectImpl :: forall point d r.
                 ( Point_ point d r, Ord r, Num r
                 ) => Box point -> Box point -> Maybe (Box (Point d r))
bx `intersectImpl` bx' = fmap fromExtent' . sequence
                         $ liftI2 intersect (extent' bx) (extent' bx')
    where
      extent' :: Box point -> Vector d (ClosedInterval r)
      extent' = extent

      fromExtent' :: Vector d (IntersectionOf (ClosedInterval r) (ClosedInterval r))
                  -> Box (Point d r)
      fromExtent' = fromExtent . over components f
      f :: IntersectionOf (ClosedInterval r) (ClosedInterval r)
        -> ClosedInterval r
      f = \case
        ClosedInterval_x_ClosedInterval_Point x     -> ClosedInterval x x
        ClosedInterval_x_ClosedInterval_Contained i -> i
        ClosedInterval_x_ClosedInterval_Partial i   -> i
