{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens
import           Data.Colour as Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (toSRGB24, RGB(..), sRGB)
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Semialign
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Graphics.Camera
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.Line.PointAndVector
import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Prelude hiding (zipWith)
import qualified System.File.OsPath as File
import           System.OsPath
import           System.ProgressBar

--------------------------------------------------------------------------------


class (r ~ NumType geom
      ) => CanComputeNormalVector geom r | geom -> r where
  {-# mINIMAL normalVectorAt #-}
  -- | Given a point on the object, and the object. Compute the outward normal vector at
  -- the given point. The normal is returned as a unit vector.
  --
  -- pre: the query point lies on (the surface of) the object. (This is not checked)
  normalUnitVectorAt     :: ( Point_ point d r
                            , Has_ Metric_ d r
                            , d ~ Dimension geom
                            , Radical r, Fractional r)
                         => point -> geom -> Vector d r
  normalUnitVectorAt q g = signorm $ normalVectorAt q g

  -- | Given a point on the object, and the object. Compute the outward normal vector at
  -- the given point. No Guarantees are given about the length of the resulting vector.
  --
  -- pre: the query point lies on (the surface of) the object. (This is not checked)
  normalVectorAt :: ( Point_ point d r, d ~ Dimension geom, Num r)
                 => point -> geom -> Vector d r

instance Point_ center d r => CanComputeNormalVector (Ball center) r where
  normalVectorAt q b = (q^.asPoint) .-. (b^.center.asPoint)

instance Point_ vertex 3 r => CanComputeNormalVector (Triangle vertex) r where
  normalVectorAt _ (Triangle u v w) = cross (v .-. u) (w .-. u)
  -- FIXME: shoudn't we check the orientation of the triangle: I think it may point the
  -- wrong way now

--------------------------------------------------------------------------------

-- | The color type we use
type Color = AlphaColour Double

-- | The type of Real numbers
type R = Double

-- | A ray; i.e. a halfline
type Ray = HalfLine (Point 3 R)

-- | Supported geometry types
data SceneGeom = ABall     (Ball (Point 3 R))
               | ATriangle (Triangle (Point 3 R))
               deriving (Show,Eq)

type instance NumType   SceneGeom = R
type instance Dimension SceneGeom = 3

instance CanComputeNormalVector SceneGeom R where
  normalVectorAt q = \case
    ABall b     -> normalVectorAt q b
    ATriangle t -> normalVectorAt q t

-- | Every object is some geometric object and a color
data SceneObject = SceneObject { _geom        :: SceneGeom
                               , _objectColor :: Color
                               }
                 deriving (Show,Eq)
makeLenses ''SceneObject

-- | A scene is just a list of objects
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

-- -- | Compute whether the ray hits the object, and if so, what the first parameter value is at
-- -- which it does so. (In addition it returns the color of the object)
-- intersectObject         :: Ray -> SceneObject -> Closest R (SceneGeom, Point 3 R :+ R)
-- intersectObject ray obj = intersectWithRay ray (obj^.geom) <&> (:+ (obj^.objectColor))

-- | Compute the closest of intersection between the ray and the object, if it exists.
-- returns he scene object togehter with the point of intersection (that itself is tagged)
-- by the intersection paramter.
intersectWithRay         :: Ray -> SceneObject -> Closest R (SceneObject, Point 3 R :+ R)
intersectWithRay ray obj = closest . fmap (\p -> p^.extra :+ (obj,p))
                         $ intersectWithRay' ray (obj^.geom)

-- | implementation of intersectWithRay
intersectWithRay'     :: Ray -> SceneGeom -> Maybe (Point 3 R :+ R)
intersectWithRay' ray = \case
   ABall b      -> ray `intersect` b <&> \case
     Line_x_Ball_Point p     -> p
     Line_x_Ball_Segment seg -> seg^.start
   ATriangle tri -> ray `intersect` tri <&> \case
     Line_x_Triangle_Point p         -> p
     Line_x_Triangle_LineSegment seg -> seg^.start


--------------------------------------------------------------------------------

-- | Determine the color of the closest object hit by the ray
rayColor     :: Ray -> Scene -> Maybe Color
rayColor ray = fmap colorAt . shootRay ray

colorAt         :: (SceneObject, Point 3 R :+ R) -> Color
colorAt (obj,q) = let Vector3 r g b = normalUnitVectorAt q (obj^.geom)
                      a = 0.5*b + 1 -- ?
                  -- in opaque $ blend a (sRGB 0.5 0.7 1) white
                  in blend a (opaque $ sRGB r g b) (obj^.objectColor)


   -- auto a = 0.5*(unit_direction.y() + 1.0);
   --  return (1.0-a)*color(1.0, 1.0, 1.0) + a*color(0.5, 0.7, 1.0);


  -- obj^.objectColor -- DO something with the normal vector here I guess.
  -- `blend`
  -- normalUnitVectorAt q _








    -- directedLineTriangleIntersect (toLine ray) tri of
    -- Just (Line_x_Triangle_Point (t,_)) | t >= 0 -> Just t
    -- _                                           -> Nothing

-- -- | Helper function; convert into an oriented line.
-- toLine :: HalfLine (Point d r) -> LinePV d r
-- toLine (HalfLine p v) = LinePV p v



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
renderPixel _ (Vector2 topLeft bottomRight) _ camera scene = toPixelRGBA $ rayColor ray scene
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

-- | We describe the viewport by its topleft corner and its bottom right corner.
type ViewPort = Vector 2 (Point 3 R)

--------------------------------------------------------------------------------
-- * Settings

-- * For the picture

-- | Background  color
backgroundColor :: Color
backgroundColor = blue `withOpacity` 0.1
  -- transparent -- transparent

-- | Number of pixels in the ouput image
outputWidth :: Int
outputWidth = 640

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
numSamplesPerPixel = 100

-- | Maximum complexity of a single ray; (in number of segments)
maxRayComplexity :: Int
maxRayComplexity = 50

-- | epsilon value; round things whose absolute value is smaller than this to zero.
epsilon :: Double
epsilon = 0.000000001

----------------------------------------
-- * The scene

theScene :: Scene
theScene = [ SceneObject (ABall $ Ball (Point3 0 3 0)     1    ) (opaque red)
           , SceneObject (ABall $ Ball (Point3 2 5 3)     (1.5)) (opaque blue)
           , SceneObject (ABall $ Ball (Point3 (-3) 20 6) 3    ) (opaque black)


           , SceneObject (ABall $ Ball (Point3 0 2 0)     0.1    ) (opaque brown)

           , SceneObject (ATriangle $ Triangle (Point3 (-6) 10 8)
                                               (Point3 (-3) 12 6)
                                               (Point3 (-5) 15 7)
                         ) (opaque purple)


           , SceneObject (ATriangle $ Triangle (Point3 (-10) 22 16)
                                               (Point3 (-6) 20 19)
                                               (Point3 (-5) 25 7.5)
                         ) (opaque pink)
           ]
           <>
           mkPlane (Rectangle (Point2 3 3) (Point2 8 11)) (-0.1) (opaque blue)
           <>
           ground

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


----------------------------------------
-- * Settings for the progress bar

-- | how frequently we refresh; every 10 units of work.
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
