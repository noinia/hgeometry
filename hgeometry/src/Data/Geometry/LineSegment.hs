{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.LineSegment
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module Data.Geometry.LineSegment
  ( LineSegment(LineSegment, LineSegment', ClosedLineSegment, OpenLineSegment)
  , endPoints

  , _SubLine
  , module Data.Geometry.Interval

  , toLineSegment
  , orderedEndPoints
  , segmentLength
  , sqSegmentLength
  , sqDistanceToSeg, sqDistanceToSegArg
  , flipSegment
  , onSegment2

  , interpolate
  ) where

import Data.Geometry.Interval hiding (width, midPoint)
import Data.Geometry.LineSegment.Internal
