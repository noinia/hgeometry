{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Wavefront
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Utilities for dealing with Wavefront files
--
--------------------------------------------------------------------------------
module Wavefront
  ( fromFile
  , allTriangles

  , elValue, elMtl
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Catch
import Codec.Wavefront.IO qualified
import Codec.Wavefront.Face qualified as Face
import Codec.Wavefront.Object (WavefrontOBJ, objFaces, objLocations
                              )
import Codec.Wavefront.Element (Element, ElementF(Element))
import Codec.Wavefront.Element qualified as Element
import Codec.Wavefront.Location
import HGeometry.Triangle
import Hiraffe.Graph.Class (HasVertices(..), HasVertices'(..))
import System.OsPath
import HGeometry.Point
import Data.Vector qualified as Vector
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | Read a Wavefront file from disk
fromFile    :: (MonadIO m, MonadThrow m) => OsPath -> m (Either String WavefrontOBJ)
fromFile fp = decodeUtf fp >>= Codec.Wavefront.IO.fromFile


--------------------------------------------------------------------------------

type instance Dimension (ElementF material geom) = Dimension geom
type instance NumType   (ElementF material geom) = NumType   geom

elValue :: Lens (ElementF material a) (ElementF material b) a b
elValue = lens Element.elValue (\el x -> el { Element.elValue = x})

elMtl :: Lens (ElementF material a) (ElementF material' a) material material'
elMtl = lens Element.elMtl (\el x -> el { Element.elMtl = x})


instance HasVertices' geom      => HasVertices' (ElementF material geom) where
  type Vertex   (ElementF material geom) = Vertex   geom
  type VertexIx (ElementF material geom) = VertexIx geom
  vertexAt u = elValue .> vertexAt u
  numVertices = numVertices . Element.elValue

instance HasVertices geom geom'
          => HasVertices (ElementF material geom) (ElementF material geom') where
  vertices = elValue .> vertices

instance Triangle_ triangle point  => Triangle_ (ElementF material triangle) point where
  corners = elValue.corners

--------------------------------------------------------------------------------


-- | Produce a vector with all Triangles in the OBJ File. This will
-- assume all faces are convex, and convert them into triangles as
-- well.
allTriangles        :: WavefrontOBJ -> [Element (Triangle (Point 3 Float))]
allTriangles objFile = foldMap (traverse toTriangle) $ objFaces objFile
  where
    toTriangle = \case
      Face.Face i j k is  -> let vi  = vtx i
                                 vj  = vtx j
                                 vk  = vtx k
                                 vis = map vtx is
                             in Triangle vi vj vk : map (Triangle vi vk) vis

    vtx i = toPoint $ objLocations objFile Vector.! (Face.faceLocIndex i)

    toPoint (Location x y z _w) = Point3 x (negate z) y
    -- we ignore the _w value, if they exist
    --
    -- TODO: not sure why, but at least for the Cornel box we seem to
    -- need to flip the y and z coordinates.
