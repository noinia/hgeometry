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
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Codec.Wavefront.IO qualified
import Codec.Wavefront.Face qualified as Face
import Codec.Wavefront (WavefrontOBJ, objFaces, objLocations)
import Codec.Wavefront.Element (elValue)
import Codec.Wavefront.Location
import HGeometry.Triangle
import HGeometry.Ext
import System.OsPath
import HGeometry.Point
import Data.Vector qualified as Vector

--------------------------------------------------------------------------------

-- | Read a Wavefront file from disk
fromFile    :: (MonadIO m, MonadThrow m) => OsPath -> m (Either String WavefrontOBJ)
fromFile fp = decodeUtf fp >>= Codec.Wavefront.IO.fromFile


--------------------------------------------------------------------------------




--------------------------------------------------------------------------------


-- | Produce a vector with all Triangles in the OBJ File. This will
-- assume all faces are convex, and convert them into triangles as
-- well.
allTriangles        :: WavefrontOBJ -> [Triangle (Point 3 Float)]

allTriangles objFile = foldMap (toTriangle . elValue) $ objFaces objFile
  where
    toTriangle = \case
      Face.Face i j k is  -> let vi  = vtx i
                                 vj  = vtx j
                                 vk  = vtx k
                                 vis = map vtx is
                             in Triangle vi vj vk : map (Triangle vi vk) vis

    vtx i = toPoint $ objLocations objFile Vector.! (Face.faceLocIndex i)

    toPoint (Location x y z _w) = Point3 x z y
    -- we ignore the _w value, if they exist
    --
    -- TODO: not sure why, but at least for the Cornel box we seem to
    -- need to flip the y and z coordinates.
