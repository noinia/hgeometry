{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper types for implementing line intersections
--
--------------------------------------------------------------------------------
module HGeometry.Line.Intersection
  ( LineLineIntersection, LineLineIntersectionG(..)
  ) where

import HGeometry.Point
import HGeometry.Properties (NumType)

--------------------------------------------------------------------------------

-- | Line x Line intersections are either just points or lines.
data LineLineIntersectionG r line = Line_x_Line_Point (Point 2 r)
                                  | Line_x_Line_Line line
                                  deriving (Show,Eq,Read,Functor)

type LineLineIntersection line = LineLineIntersectionG (NumType line) line


-- deriving instance (Show (Point 2 (NumType line)), Show line) => Show (LineLineIntersection line)
-- deriving instance (Eq (Point 2 (NumType line)), Eq line)     => Eq (LineLineIntersection line)
