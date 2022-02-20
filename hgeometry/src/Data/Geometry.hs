
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Basic Geometry Types
--
--------------------------------------------------------------------------------
module Data.Geometry( module Data.Geometry.Properties
                    , module Data.Geometry.Transformation
                    , module Data.Geometry.Point
                    , module V
                    , module Data.Geometry.Line
                    , module Data.Geometry.LineSegment
                    , module Data.Geometry.PolyLine
                    , module Data.Geometry.Polygon
                    -- , module Linear.Affine
                    -- , module Linear.Vector
                    ) where

import Data.Geometry.Vector as V hiding (last)
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.PolyLine hiding (fromPoints)
import Data.Geometry.Polygon hiding (fromPoints)
import Data.Geometry.Properties
import Data.Geometry.Transformation
-- import Linear.Affine hiding (Point, Vector, origin)
-- import Linear.Vector

--------------------------------------------------------------------------------
