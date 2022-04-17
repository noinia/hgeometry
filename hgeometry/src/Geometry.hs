
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Basic Geometry Types
--
--------------------------------------------------------------------------------
module Geometry
  ( module Geometry.Properties
  , module Geometry.Transformation
  , module Geometry.Point
  , module V
  , module Geometry.Line
  , module Geometry.LineSegment
  , module Geometry.PolyLine
  , module Geometry.Polygon
    -- , module Linear.Affine
    -- , module Linear.Vector
  ) where

import Geometry.Vector as V hiding (last)
import Geometry.Line
import Geometry.LineSegment
import Geometry.Point
import Geometry.PolyLine hiding (fromPoints)
import Geometry.Polygon hiding (fromPoints)
import Geometry.Properties
import Geometry.Transformation
-- import Linear.Affine hiding (Point, Vector, origin)
-- import Linear.Vector

--------------------------------------------------------------------------------
