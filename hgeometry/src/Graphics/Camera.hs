{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Camera
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Data type to represent a camera and some functions for working with it.
--
--------------------------------------------------------------------------------
module Graphics.Camera( Camera(Camera)
                      , cameraPosition, rawCameraNormal, rawViewUp
                      , viewPlaneDepth, nearDist, farDist, screenDimensions

                      , cameraNormal, viewUp

                      , cameraTransform, worldToView

                      , toViewPort, perspectiveProjection, rotateCoordSystem
                      , flipAxes

                      ) where

import Control.Lens
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Transformation

--------------------------------------------------------------------------------

-- | A basic camera data type. The fields stored are:
--
-- * the camera position,
-- * the raw camera normal, i.e. a unit vecotr into the center of the screen,
-- * the raw view up vector indicating which side points "upwards" in the scene,
-- * the viewplane depth (i.e. the distance from the camera position to the plane on which we project),
-- * the near distance (everything closer than this is clipped),
-- * the far distance (everything further away than this is clipped), and
-- * the screen dimensions.
--
data Camera r = Camera { _cameraPosition   :: !(Point 3 r)
                       , _rawCameraNormal  :: !(Vector 3 r)
                         -- ^ unit vector from camera into center of the screen
                       , _rawViewUp        :: !(Vector 3 r)
                       -- ^ viewUp; assumed to be unit vector
                       , _viewPlaneDepth   :: !r
                       , _nearDist         :: !r
                       , _farDist          :: !r
                       , _screenDimensions :: !(Vector 2 r)
                       } deriving (Show,Eq,Ord)

----------------------------------------
-- * Field Accessor Lenses

makeLenses ''Camera

--------------------------------------------------------------------------------
-- * Accessor Lenses

-- | Lens to get and set the Camera normal, makes sure that the vector remains
-- normalized.
cameraNormal :: Floating r => Lens' (Camera r) (Vector 3 r)
cameraNormal = lens _rawCameraNormal (\c n -> c { _rawCameraNormal = signorm n} )


-- | Lens to get and set the viewUp vector. Makes sure the vector remains
-- normalized.
viewUp :: Floating r => Lens' (Camera r) (Vector 3 r)
viewUp = lens _rawViewUp (\c n -> c { _rawViewUp = signorm n})


--------------------------------------------------------------------------------
-- * Camera Transformation functions


-- | Full transformation that renders the figure
cameraTransform   :: Fractional r => Camera r -> Transformation 3 r
cameraTransform c =  toViewPort c
                 |.| perspectiveProjection c
                 |.| worldToView c

-- | Translates world coordinates into view coordinates
worldToView   :: Fractional r => Camera r -> Transformation 3 r
worldToView c =  rotateCoordSystem c
             |.| (translation $ (-1) *^ c^.cameraPosition.vector)

-- | Transformation into viewport coordinates
toViewPort   :: Fractional r => Camera r -> Transformation 3 r
toViewPort c = Transformation . Matrix
             $ Vector4 (Vector4 (w/2) 0     0     0)
                       (Vector4 0     (h/2) 0     0)
                       (Vector4 0     0     (1/2) (1/2))
                       (Vector4 0     0     0     1)
  where
    Vector2 w h = c^.screenDimensions


-- | constructs a perspective projection
perspectiveProjection   :: Fractional r => Camera r -> Transformation 3 r
perspectiveProjection c = Transformation . Matrix $
    Vector4 (Vector4 (-n/rx) 0       0              0)
            (Vector4 0       (-n/ry) 0              0)
            (Vector4 0       0       (-(n+f)/(n-f)) (-2*n*f/(n-f)))
            (Vector4 0       0       1              0)
  where
    n = c^.nearDist
    f = c^.farDist
    Vector2 rx ry = (/2) <$> c^.screenDimensions

-- | Rotates coordinate system around the camera, such that we look in the negative z
-- direction
rotateCoordSystem   :: Num r => Camera r -> Transformation 3 r
rotateCoordSystem c = rotateTo $ Vector3 u v n
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera




-- transformBy' (Transformation m) (Vector3 x y z) = m `mult` (Vector4 x y z (-z))

-- | Flips the y and z axis.
flipAxes :: Num r => Transformation 3 r
flipAxes = Transformation . Matrix
             $ Vector4 (Vector4 1 0 0 0)
                       (Vector4 0 0 1 0)
                       (Vector4 0 1 0 0)
                       (Vector4 0 0 0 1)
