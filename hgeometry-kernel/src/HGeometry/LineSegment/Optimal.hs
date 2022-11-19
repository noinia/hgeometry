{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Optimal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Optimal
  (

  ) where


import Control.Lens
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Optimal
import HGeometry.LineSegment.Class
import HGeometry.Properties
import HGeometry.Vector


--------------------------------------------------------------------------------

-- | Data type representing intervals
newtype LineSegment endPoint point = MkLineSegment (Interval endPoint point)

type ClosedLineSegment point = LineSegment (EndPoint Closed) point
type OpenLineSegment point   = LineSegment (EndPoint Open) point

-- | Construct a line Segment
pattern LineSegment     :: OptCVector_ 2 (endPoint point)
                        => endPoint point -> endPoint point -> LineSegment endPoint point
pattern LineSegment p q = MkLineSegment (Interval p q)

-- | Construct a closed interval
pattern ClosedLineSegment     :: OptCVector_ 2 point
                              =>  point -> point -> ClosedLineSegment point
pattern ClosedLineSegment s t = LineSegment (ClosedE s) (ClosedE t)

-- | Construct an open ended interval
pattern OpenLineSegment     :: OptCVector_ 2 point
                            =>  point -> point -> OpenLineSegment point
pattern OpenLineSegment s t = LineSegment (OpenE s) (OpenE t)

type instance NumType   (LineSegment endPoint point) = NumType point
type instance Dimension (LineSegment endPoint point) = Dimension point
