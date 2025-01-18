{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens
import           Data.Coerce
import           Data.Colour as Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (toSRGB24, RGB(..))
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Semialign
import           Debug.Trace
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Graphics.Camera
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.Line.PointAndVector
import           HGeometry.Point
import           HGeometry.Triangle
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Prelude hiding (zipWith)
import qualified System.File.OsPath as File
import           System.OsPath
import           System.ProgressBar

--------------------------------------------------------------------------------

type Color = AlphaColour Double

type R = Double

type Ray = HalfLine (Point 3 R)


data SceneGeom = ABall     (Ball (Point 3 R))
               | ATriangle (Triangle (Point 3 R))
               deriving (Show,Eq)

data SceneObject = SceneObject { _geom       :: SceneGeom
                               , _objectColor :: Color
                               }
                 deriving (Show,Eq)
makeLenses ''SceneObject

type Scene = [SceneObject]




--------------------------------------------------------------------------------
{-
-- | Given a rectangle, and a vector specifying the number of columns and the number of
-- rows, returns a bunch of equal size subdivided rectangles. The result is given as
-- list of *rows* (from top to bottom). Each row from left to right.
subdivide                                                :: (Point_ point 2 r, Fractional r)
                                                          => Rectangle point -> Vector 2 Int
                                                          -> [[Rectangle (Point 2 r)]]
subdivide rect@(Rectangle bl _) (Vector2 numCols numRows) =
    [ [ mkRect c r | c <- [0..(numCols - 1)] ] | r <- [0..(numRows - 1)] ]
  where
    Point2 x y = bl^.asPoint
    Vector2 w h = size rect
    w' = w / fromIntegral numCols
    h' = h / fromIntegral numRows
    mkRect c r = let i = fromIntegral c
                     j = fromIntegral (numRows-r)
                 in Rectangle (Point2 (x + (i*w'))   (y + (j-1)*h'))
                              (Point2 (x + (i+1)*w') (y + j    *h'))


testSubdivide = subdivide (Rectangle (Point2 10 4) (Point2 20 20)) (Vector2 2 4)
-}

--------------------------------------------------------------------------------
-- * The Ray shooting stuff


-- -- | Intersect to try and get the color
-- ballColor            :: Ray -> Ball (Point 3 R) :+ Color -> Maybe Color
-- ballColor r (b :+ c)
--   | r `intersects` b = Just c
--   | otherwise        = Nothing


newtype Closest r a = Closest (Top (r :+ a))
                    deriving (Show)

instance Eq r => Eq (Closest r a) where
  (Closest x) == (Closest y) = (view core <$> x) == (view core <$> y)
instance Ord r => Ord (Closest r a) where
  (Closest x) `compare` (Closest y) = (view core <$> x) `compare` (view core <$> y)

instance Ord r => Semigroup (Closest r a) where
  x <> y = min x y
instance Ord r => Monoid (Closest r a) where
  mempty = Closest Top

closest :: Maybe (r :+ a) -> Closest r a
closest = Closest . view (from _TopMaybe)

getClosest :: Closest r a -> Maybe (r :+ a)
getClosest = view (coerced._TopMaybe)

-- | Determine the color of the closest object hit by the ray
shootRay   :: Ray -> Scene -> Maybe Color
shootRay r = fmap (view extra) . getClosest . foldMap (closest . intersectObject r)

-- | Compute whether the ray hits the object, and if so, what the first parameter value is at
-- which it does so. (In addition it returns the color of the object)
intersectObject         :: Ray -> SceneObject -> Maybe (R :+ Color)
intersectObject ray obj = intersectWithRay ray (obj^.geom) <&> (:+ (obj^.objectColor))

-- | Compute the time of intersection between the ray and the scene, if it exists.
intersectWithRay     :: Ray -> SceneGeom -> Maybe R
intersectWithRay ray = \case
  ABall b    | ray `intersects` b -> Just 0 -- TODO, ifx this ...
             | otherwise -> Nothing
  ATriangle tri -> case directedLineTriangleIntersect (toLine ray) tri of
    Just (Line_x_Triangle_Point (t,_)) | t >= 0 -> Just t
    _                                           -> Nothing

toLine (HalfLine p v) = LinePV p v

-- | Renders a given pixel.
renderPixel                         :: Point 2 Int
                                    -- ^ The pixel coordinates
                                    -> ViewPort
                                    -- ^ The part of the viewport corresponding to this pixel.
                                    -> Vector 2 (Vector 3 R)
                                    -- ^ pixel dimensions
                                    -> Camera R
                                    -> Scene
                                    -> PixelRGBA8
renderPixel _ (Vector2 topLeft bottomRight) _ camera scene = toPixelRGBA $ shootRay ray scene
  where
    cameraPos = camera^.cameraPosition
    ray       = HalfLine cameraPos (midPoint .-. cameraPos)
    midPoint  = topLeft .+^ ((bottomRight .-. topLeft) ^/ 2)

toPixelRGBA    :: Maybe Color -> PixelRGBA8
toPixelRGBA mc = let c = fromMaybe backgroundColor mc
                 in case toSRGB24 $ c `Colour.over` black of
                      RGB r g b -> PixelRGBA8 r g b (round $ 255 * alphaChannel c)

-- | Computes the part of the viewport (i.e. some rectangle floating in world space)
-- corresponding to the pixel x,y
pixelViewPort :: Point 3 R -> Vector 2 (Vector 3 R) -> Int -> Int -> ViewPort
pixelViewPort topLeft (Vector2 xVec yVec) x' y' =
  let x = fromIntegral x'
      y = fromIntegral y'
  in Vector2 (topLeft .+^ ((x      *^ xVec) ^+^ (y    *^ yVec)))
             (topLeft .+^ (((x +1) *^ xVec) ^+^ ((y+1)*^ yVec)))


-- containsMid (Point3 qx _ qz) (Vector2 (Point3 tlx _ tlz)
--                                       (Point3 brx _ brz)
--                              ) = tlx <= qx && qx <= brx &&
--                                  tlz >= qz && qz >= brz

-- | Render the Scene, while reporting progress
renderWithProgress :: IO () -> Vector 2 Int -> Camera R -> Scene -> IO (Image PixelRGBA8)
renderWithProgress reportProgress screenDims@(Vector2 w h) camera scene = do
    -- print ("ratio screen", fromIntegral w/ fromIntegral h)
    -- print ("ratio dims", camera^.viewportDimensions.xComponent
    --       , camera^.viewportDimensions.yComponent)
    print ("screenDims",screenDims)
    print ("VP size",camera^.viewportDimensions)
    print ("VP dims",viewportDims)
    print ("midPoint",midPoint)
    print ("topLeft",topLeft)
    print ("viewPort",theViewport)
    print ("pixelDims",pixelDims)
    withImage w h $ \x y -> let !pix = renderPixel (Point2 x y)
                                                   (pixelViewPort topLeft pixelDims x y)
                                                   pixelDims
                                                   camera scene
                            in pix <$ reportProgress
  where
    -- midpoint of the viewport (in 3D posiiton)
    midPoint = (camera^.cameraPosition) .+^ ((camera^.focalDepth) *^ camera^.rawCameraNormal)

    -- xVec is a vector (in R^3) that corresponds to the x-axis in the viewPlane.
    -- the magnitude of the vector is so that it corresponds to the width of the viewPort
    -- Same for the yVec. Note that in the viewPlane the y-axis runs "downward", so
    --
    -- it directly corresponds to the the pixel coordinates.
    viewportDims@(Vector2 xVec yVec) =
      zipWith (*^) (camera^.viewportDimensions)
                   (Vector2 ((camera^.cameraNormal) `cross` (camera^.viewUp))
                            (negated $ camera^.viewUp))

    -- the topleft point of the viewport
    topLeft = midPoint .-^ ((xVec ^+^ yVec) ^/ 2)

    -- the dimensions of a single pixel in viewPort
    pixelDims = zipWith (\axis numPixels -> axis ^/ fromIntegral numPixels)
                        viewportDims
                        screenDims

    theViewport = Vector2 topLeft (topLeft .+^ (xVec ^+^ yVec))

type ViewPort = Vector 2 (Point 3 R)

--------------------------------------------------------------------------------
-- * Settings

-- * For the picture

-- backgroundColor :: PixelRGBA8
backgroundColor :: Color
backgroundColor = green `withOpacity` 0.8
  -- transparent -- transparent

outputWidth :: Int
outputWidth = 640

aspectRatio :: Rational
aspectRatio = 16 / 9

-- | Size of the output picture
outputDimensions :: Vector 2 Int
outputDimensions = Vector2 outputWidth (ceiling $ fromIntegral outputWidth / aspectRatio)




----------------------------------------
-- * Settings related to the Camera

theCamera :: Camera R
theCamera = def&viewportDimensions .~ fromDesiredHeight 2

-- | Computes the viewportDimensions from a given desired height.
fromDesiredHeight               :: Fractional r => r -> Vector 2 r
fromDesiredHeight desiredHeight = let Vector2 w h = fromIntegral <$> outputDimensions
                                  in Vector2 (desiredHeight * (w/h)) desiredHeight


----------------------------------------
-- * The scene

theScene :: Scene
theScene = [ SceneObject (ABall $ Ball (Point3 0 3 0)     1    ) (opaque red)
           , SceneObject (ABall $ Ball (Point3 2 5 3)     (1.5)) (opaque blue)
           , SceneObject (ABall $ Ball (Point3 (-3) 20 6) 3    ) (opaque black)

           , SceneObject (ATriangle $ Triangle (Point3 (-6) 10 8)
                                               (Point3 (-3) 12 6)
                                               (Point3 (-5) 15 7)
                         ) (opaque purple)


           , SceneObject (ATriangle $ Triangle (Point3 (-10) 22 16)
                                               (Point3 (-6) 20 19)
                                               (Point3 (-5) 25 7.5)
                         ) (opaque pink)
           ]

----------------------------------------
-- * Settings for the progress bar

refreshRate :: Double
refreshRate = 10

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let amountOfWork (Vector2 w h) = w * h
      initialProgress = Progress 0 (amountOfWork outputDimensions) ()
  progressBar <- newProgressBar defStyle refreshRate initialProgress

  imageData <- renderWithProgress (incProgress progressBar 1)
                                  outputDimensions theCamera theScene

  let bs = encodePng imageData
  File.writeFile [osp|foo.png|] bs
