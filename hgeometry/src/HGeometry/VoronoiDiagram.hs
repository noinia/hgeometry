--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.VoronoiDiagram
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Voronoi Diagrams
--
--------------------------------------------------------------------------------
module HGeometry.VoronoiDiagram
  ( VoronoiDiagram(..)
  , voronoiDiagram
  , voronoiVertices
  , edgeGeometries
  ) where

import HGeometry.VoronoiDiagram.ViaLowerEnvelope
