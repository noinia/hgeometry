--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Graphics.Camera
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Data type to represent a camera and some functions for working with it.
--
--------------------------------------------------------------------------------
module HGeometry.Graphics.Camera
  ( Camera(Camera)
  , cameraPosition, rawCameraNormal, rawViewUp
  , focalDepth, nearDist, farDist, viewportDimensions

  , cameraNormal, viewUp

  , cameraTransform, worldToView

  , toViewPortTransform, perspectiveProjection, rotateCoordSystem
  , flipAxes

  ) where

import Control.Lens
import Data.Default.Class
import HGeometry.Matrix
import HGeometry.Number.Radical
import HGeometry.Point
import HGeometry.Transformation
import HGeometry.Vector
--------------------------------------------------------------------------------

-- | A basic camera data type. The fields stored are:
--
-- * the camera position,
-- * the raw camera normal, i.e. a unit vector into the center of the screen,
-- * the raw view up vector indicating which side points "upwards" in the scene,
-- * the focal depth (i.e. the distance from the camera position to the plane on which we project),
-- * the near distance (everything closer than this is clipped),
-- * the far distance (everything further away than this is clipped), and
-- * the viewport dimensions; i.e. the viewport (on the view plane) on which we draw
--
data Camera r = Camera { _cameraPosition     :: !(Point 3 r)
                       , _rawCameraNormal    :: !(Vector 3 r)
                         -- ^ unit vector from camera into center of the screen
                       , _rawViewUp          :: !(Vector 3 r)
                       -- ^ viewUp; assumed to be unit vector
                       , _focalDepth         :: !r
                       , _nearDist           :: !r
                       , _farDist            :: !r
                       , _viewportDimensions :: !(Vector 2 r)
                       -- ^ Dimensions of the camera viewport; in world coordinates
                       } deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

instance Num r => Default (Camera r) where
  -- ^ A default camera, placed at the origin, looking along the y-axis. The view-up
  -- vector is the z-axis. The focalDepth is 1.
  def = Camera { _cameraPosition     = origin
               , _rawCameraNormal    = Vector3 0 1 0 -- We are looking itnto the y-direction
               , _rawViewUp          = Vector3 0 0 1 -- up in the z-direction
               , _focalDepth         = 1
               , _nearDist           = 0
               , _farDist            = 100
               , _viewportDimensions = Vector2 4 3
               }

----------------------------------------
-- * Field Accessor Lenses

-- Lemmih: Writing out the lenses by hand so they can be documented.
-- makeLenses ''Camera

-- | Camera position.
cameraPosition :: Lens' (Camera r) (Point 3 r)
cameraPosition = lens _cameraPosition (\cam p -> cam {_cameraPosition=p})

-- | Raw camera normal, i.e. a unit vector into the center of the screen.
rawCameraNormal :: Lens' (Camera r) (Vector 3 r)
rawCameraNormal = lens _rawCameraNormal (\cam r -> cam {_rawCameraNormal=r})

-- | Raw view up vector indicating which side points "upwards" in the scene.
rawViewUp :: Lens' (Camera r) (Vector 3 r)
rawViewUp = lens _rawViewUp (\cam r -> cam {_rawViewUp=r})

-- | The focal length (i.e. distance from the camera position to the plane on which we
-- project.) Also known as viewplane depth.
focalDepth :: Lens' (Camera r) r
focalDepth = lens _focalDepth (\cam v -> cam {_focalDepth=v})

-- | Near distance (everything closer than this is clipped).
nearDist :: Lens' (Camera r) r
nearDist = lens _nearDist (\cam n -> cam {_nearDist=n})

-- | Far distance (everything further away than this is clipped).
farDist :: Lens' (Camera r) r
farDist = lens _farDist (\cam f -> cam {_farDist=f})

-- | The viewport dimensions.
viewportDimensions :: Lens' (Camera r) (Vector 2 r)
viewportDimensions = lens _viewportDimensions (\cam d -> cam {_viewportDimensions=d})

--------------------------------------------------------------------------------
-- * Accessor Lenses

-- | Lens to get and set the Camera normal, makes sure that the vector remains
-- normalized.
cameraNormal :: (Radical r, Fractional r) => Lens' (Camera r) (Vector 3 r)
cameraNormal = lens _rawCameraNormal (\c n -> c { _rawCameraNormal = signorm n} )


-- | Lens to get and set the viewUp vector. Makes sure the vector remains
-- normalized.
viewUp :: (Radical r, Fractional r) => Lens' (Camera r) (Vector 3 r)
viewUp = lens _rawViewUp (\c n -> c { _rawViewUp = signorm n})


--------------------------------------------------------------------------------
-- * Camera Transformation functions


-- | Full transformation that renders the figure
cameraTransform   :: Fractional r => Camera r -> Transformation 3 r
cameraTransform c =  toViewPortTransform c
                 |.| perspectiveProjection c
                 |.| worldToView c

-- | Translates world coordinates into view coordinates
worldToView   :: Fractional r => Camera r -> Transformation 3 r
worldToView c = rotateCoordSystem c |.| translation ((-1) *^ c^.cameraPosition.vector)

-- | Transformation into viewport coordinates
toViewPortTransform   :: Fractional r => Camera r -> Transformation 3 r
toViewPortTransform c = Transformation . Matrix
                      $ Vector4 (Vector4 (w/2) 0     0     0)
                                (Vector4 0     (h/2) 0     0)
                                (Vector4 0     0     (1/2) (1/2))
                                (Vector4 0     0     0     1)
  where
    Vector2 w h = c^.viewportDimensions

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
    Vector2 rx ry = (/2) <$> c^.viewportDimensions

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
