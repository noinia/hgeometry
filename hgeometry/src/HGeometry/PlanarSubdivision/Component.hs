--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlnarSubdivision.Component
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Connected Components in a PlanarSubdivision
--
--------------------------------------------------------------------------------
module HGeometry.PlanarSubdivision.Component
  ( Component
  ) where

import HGeometry.PlanarSubdivision.Raw
import HGeometry.PlaneGraph.Type
import Hiraffe.PlanarGraph (FaceId, VertexId)
import Hiraffe.PlanarGraph.Dart (Dart)

--------------------------------------------------------------------------------

-- | A connected component.
--
-- For every face f, and every hole in this face, the facedata points to a dart
-- d on the hole s.t. this dart has the face f on its left. i.e.
-- leftFace d = f
type Component s = PlaneGraph (Wrap s) (VertexId s) (Dart s) (FaceId s)
