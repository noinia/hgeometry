module Settings where

import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB (sRGB)
import Data.Default.Class
import HGeometry.Graphics.Camera
import HGeometry.Vector
import Types

--------------------------------------------------------------------------------
-- * Settings

-- * For the picture

-- | Background  color
backgroundColor :: Color
backgroundColor = opaque $ sRGB 0.5 0.7 1
  -- blue `withOpacity` 0.1
  -- transparent -- transparent

-- | Number of pixels in the ouput image
outputWidth :: Int
outputWidth = 400 -- 640

-- | Aspect ratio of the output image
aspectRatio :: Rational
aspectRatio = 16 / 9

-- | Size of the output picture; computed from outputWith and aspect ratio
outputDimensions :: Vector 2 Int
outputDimensions = Vector2 outputWidth (ceiling $ fromIntegral outputWidth / aspectRatio)

----------------------------------------
-- * Settings related to the Camera

theCamera :: Camera R
theCamera = def&viewportDimensions .~ fromDesiredHeight 2
               -- &cameraPosition     .~ Point3 0 0 1

-- | Computes the viewportDimensions from a given desired height.
fromDesiredHeight               :: Fractional r => r -> Vector 2 r
fromDesiredHeight desiredHeight = let Vector2 w h = fromIntegral <$> outputDimensions
                                  in Vector2 (desiredHeight * (w/h)) desiredHeight


----------------------------------------

-- | The number of samples we take for each pixel
numSamplesPerPixel :: Int
numSamplesPerPixel = 20 -- 100 --20 -- 100 -- 20 -- 100

-- | Maximum complexity of a single ray; (in number of segments)
maxRayComplexity = 10 -- 50
maxRayComplexity :: Int


----------------------------------------
-- * Settings for the progress bar

-- | how frequently we refresh; in Hertz.
refreshRate :: Double
refreshRate = 2
