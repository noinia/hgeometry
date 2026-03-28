--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.PossiblyDegenerate
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Type representing degenerate linesegments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.PossiblyDegenerate
  ( PossiblyDegenerateSegment(..)
  ) where

-- import HGeometry.LineSegment.Internal
-- import HGeometry.LineSegment.Class
-- import HGeometry.Point

--------------------------------------------------------------------------------

-- | A line segment that may degenerate into a single point of type 'vertex'
data PossiblyDegenerateSegment point segment = SinglePoint point
                                             | ActualSegment segment
  deriving stock (Show,Eq,Ord,Functor,Foldable,Traversable)
