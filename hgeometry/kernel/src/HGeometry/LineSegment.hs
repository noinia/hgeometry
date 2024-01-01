{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment
  ( LineSegment(LineSegment, ClosedLineSegment, OpenLineSegment)
  , ClosedLineSegment
  , OpenLineSegment
  , EndPoint(EndPoint,OpenE,ClosedE)
  , AnEndPoint(AnEndPoint,AnOpenE,AnClosedE)
  , module HGeometry.LineSegment.Class
  , spanIn
  , EndPoint_(..)
  --
  , LineLineSegmentIntersection(..)
  , LineSegmentLineSegmentIntersection(..)
  -- , spansIntersect
  ) where

import HGeometry.LineSegment.Class
import HGeometry.LineSegment.Internal
import HGeometry.LineSegment.Intersection
