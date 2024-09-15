--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.VoronoiDiagram
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Voronoi Diagrams of points in the plane
--
--------------------------------------------------------------------------------
module HGeometry.VoronoiDiagram
  ( VoronoiDiagram(..)
  , VoronoiDiagram'(..)
  , voronoiDiagram
  , voronoiVertices
  ) where

import HGeometry.VoronoiDiagram.ViaLowerEnvelope
