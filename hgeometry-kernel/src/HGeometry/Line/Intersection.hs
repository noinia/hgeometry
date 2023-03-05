{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper types for implementin line intersections
--
--------------------------------------------------------------------------------
module HGeometry.Line.Intersection
  ( LineLineIntersection(..)
  ) where

import HGeometry.Point
import HGeometry.Properties (NumType)

--------------------------------------------------------------------------------

-- | Line x Line intersections are either just points or lines.
data LineLineIntersection line = Line_x_Line_Point (Point 2 (NumType line))
                               | Line_x_Line_Line line


deriving instance (Show (Point 2 (NumType line)), Show line) => Show (LineLineIntersection line)
deriving instance (Eq (Point 2 (NumType line)), Eq line)     => Eq (LineLineIntersection line)
