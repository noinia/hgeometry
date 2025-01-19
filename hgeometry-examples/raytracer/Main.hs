{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens
import           Control.Monad (replicateM, (<=<), (=<<))
import           Data.Colour as Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (toSRGB24, RGB(..), sRGB)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Semialign
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Direction
import           HGeometry.Ext
import           HGeometry.Graphics.Camera
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Prelude hiding (zipWith)
import           Settings
import qualified System.File.OsPath as File
import           System.OsPath
import           System.ProgressBar
import           System.Random.Stateful
import           Types

import           Debug.Trace

--------------------------------------------------------------------------------
-- * Move this into HGeometry proper
----------------------------------------








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

-- | Helper type to help compute the closest point hit by a ray
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

-- | Construct a closest value
closest :: Maybe (r :+ a) -> Closest r a
closest = Closest . view (from _TopMaybe)

-- | Get the closest intersection out; if there exists one.
getClosest :: Closest r a -> Maybe (r :+ a)
getClosest = view (coerced._TopMaybe)

-- | Compute the object that is first hit by the ray, and its intersection point.
shootRay   :: Ray -> Scene -> Maybe (SceneObject, Point 3 R :+ R)
shootRay r = fmap (view extra) . getClosest . foldMap (intersectWithRay r)

-- | Compute the closest of intersection between the ray and the object, if it exists.
-- returns he scene object togehter with the point of intersection (that itself is tagged)
-- by the intersection paramter.
intersectWithRay         :: Ray -> SceneObject -> Closest R (SceneObject, Point 3 R :+ R)
intersectWithRay ray obj = closest . fmap (\p -> p^.extra :+ (obj,p))
                         $ ray `intersect` (obj^.geom)

--------------------------------------------------------------------------------

-- | Determine the color of the closest object hit by the ray
rayColor           :: StatefulGen gen m
                   => gen
                   -> Ray
                   -> RayDepth
                   -- ^ Number of remaining bounces
                   -> Scene -> m Color
rayColor gen = rayColor'
  where
    rayColor' ray depth scene
      | depth == 0 = pure $ opaque black -- stop tracing the ray
      | otherwise  = case shootRay ray scene of
          Nothing      -> pure backgroundColor
          Just (obj,q) -> do
                             let normal = normalUnitVectorAt q (obj^.geom)
                             v <- uniformUpwardDirectionWrt normal gen
                             -- shoot a new ray
                             (blend 0.5 (opaque black))
                               <$> rayColor' (HalfLine (q^.core) v) (depth-1) scene

-- -- | Compute the color of the object at the intersection point with the ray
-- colorAt             ::

--   Ray -> (SceneObject, Point 3 R :+ R) -> Color
-- colorAt ray (obj,q) = let normal = normalUnitVectorAt q (obj^.geom)
--                          -- v <- uniformUpwardDirectionWrt normal
--                           Vector3 r g b = 0.5 *^ (normal ^+^ Vector3 1 1 1)
--                       in opaque $ sRGB r g b
--                          -- the values in the normal vec are in the range [-1,1]
--                          -- make them into range [0,1]; then interpret them as colors


--------------------------------------------------------------------------------


-- | Renders a given pixel.
renderPixel                         :: StatefulGen gen m
                                    => gen
                                    -> PixelInfo
                                    -> Camera R
                                    -> Scene
                                    -> m PixelRGBA8
renderPixel gen pixel camera scene = toPixelRGBA <$> pixelColor
  where
    pixelColor = do
      samples <- sampleUnitSquare gen
      averageColor <$> mapM (\sample ->
                                rayColor gen (ray sample) maxRayComplexity scene
                            ) samples

    cameraPos = camera^.cameraPosition
    topLeft = pixel^.pixelViewport.component @0

    ray p = let Vector2 xOfset yOfset = zipWith (*^) (p^.vector) (pixel^.pixelDimensions)
                p'    = topLeft .+^ (xOfset ^+^ yOfset)
            in HalfLine cameraPos (p' .-. cameraPos)

-- | Computes the average color
averageColor               :: (AffineSpace color, Fractional a) => NonEmpty (color a) -> color a
averageColor (c :| colors) = let n = fromIntegral $ 1 + length colors
                             in affineCombo ((1/n,) <$> colors) c

toPixelRGBA   :: Color -> PixelRGBA8
toPixelRGBA c = case toSRGB24 $ c `Colour.over` black of
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
    -- print ("screenDims",screenDims)
    -- print ("VP size",camera^.viewportDimensions)
    -- print ("VP dims",viewportDims)
    -- print ("midPoint",midPoint)
    -- print ("topLeft",topLeft)
    -- print ("viewPort",theViewport)
    -- print ("pixelDims",pixelDims)
    gen <- newIOGenM =<< getStdGen
    withImage w h $ \x y -> do
      let pix       = PixelInfo (Point2 x y) (pixelViewPort topLeft pixelDims x y) pixelDims
      !pixColor <- renderPixel gen pix camera scene
      pixColor <$ reportProgress

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


--------------------------------------------------------------------------------

-- | Generate samples in a unit square.
sampleUnitSquare     :: StatefulGen gen m => gen -> m (NonEmpty (Point 2 R))
sampleUnitSquare gen = NonEmpty.fromList
                    <$> replicateM (max 1 numSamplesPerPixel) (uniformRM ( Point $ pure 0
                                                                         , Point $ pure 1
                                                                         ) gen)


----------------------------------------
-- * The scene

theScene :: Scene
theScene = [ SceneObject (ABall $ Ball (Point3 0 3 0)     1    ) (opaque red)
           -- , SceneObject (ABall $ Ball (Point3 2 5 3)     (1.5)) (opaque blue)
           -- , SceneObject (ABall $ Ball (Point3 (-3) 20 6) 3    ) (opaque black)


           -- , SceneObject (ABall $ Ball (Point3 0 2 0)     0.1    ) (opaque brown)

           -- , SceneObject (ATriangle $ Triangle (Point3 (-6) 10 8)
           --                                     (Point3 (-3) 12 6)
           --                                     (Point3 (-5) 15 7)
           --               ) (opaque purple)


           -- , SceneObject (ATriangle $ Triangle (Point3 (-10) 22 16)
           --                                     (Point3 (-6) 20 19)
           --                                     (Point3 (-5) 25 7.5)
           --               ) (opaque pink)
           ]
           <>
           ground
           -- <>
           -- mkPlane (Rectangle (Point2 3 3) (Point2 8 11)) (-0.1) (opaque blue)

-- | Some base plane representing the gorund
ground :: [SceneObject]
ground = mkPlane (Rectangle (Point2 minX minY) (Point2 maxX maxY)) z groundColor
  where
    groundColor = opaque green
    z    = -1
    minX = -100
    maxX = 100
    minY = -100
    maxY = 10

-- | Constructs a horizontal colored rectangle floating in space
mkPlane                               :: Rectangle (Point 2 R) -> R -> Color -> [SceneObject]
mkPlane (Rectangle (Point2 minX minY)
        (Point2 maxX maxY)) z color   =
  [ SceneObject (ATriangle $ Triangle (Point3 minX minY z)
                                      (Point3 maxX minY z)
                                      (Point3 maxX maxY z)
                ) color
  , SceneObject (ATriangle $ Triangle (Point3 maxX maxY z)
                                      (Point3 minX maxY z)
                                      (Point3 minX minY z)
                ) color
  ]



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




--------------------------------------------------------------------------------
