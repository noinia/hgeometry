--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlnarSubdivision.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Type class representing planar subdivisions
--
--------------------------------------------------------------------------------
module HGeometry.PlanarSubdivision.Class
  ( PlanarSubdivision_(..)
  ) where

import Control.Lens
import Data.Coerce
import Data.Default.Class
import Data.Foldable1
import Data.Functor.Apply
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Ord (comparing)
import HGeometry.Ext
import HGeometry.LineSegment
import HGeometry.PlaneGraph.Class
import HGeometry.Point
import HGeometry.Polygon.Simple
import HGeometry.Properties
import HGeometry.Vector
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph.Class

--------------------------------------------------------------------------------

-- | A class representing Planar Subdivisions
class ( PlaneGraph_ planarSubdivision vertex
      ) => PlanarSubdivision_ planarSubdivision vertex | planarSubdivision -> vertex where





-- class HasComponents components
