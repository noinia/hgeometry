{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Common where

import           Codec.Picture             (PixelRGBA8 (..))
import           Control.Lens
import           Control.Monad.Random
import qualified Data.Foldable             as F
import           Data.List                 (nub, transpose)
import           Data.Ord
import           Data.Ratio
import qualified Data.Vector               as V
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Circular.Util as CV
import qualified Data.Vector.Unboxed       as VU
import           Graphics.SvgTree          (LineJoin (..))
import           Linear.V2                 (V2 (V2))
import           Linear.Vector             (Additive (lerp))
import           Reanimate
import           Reanimate.Animation       (Sync (SyncFreeze))

import Algorithms.Geometry.SSSP
import Data.Ext
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.Box (Rectangle, box, minPoint, maxPoint)
import Data.Geometry.Transformation
import Data.Geometry.Vector

pCenter :: (Fractional r, Ord r) => SimplePolygon p r -> Point 2 r
pCenter p = Point2 (minX + (maxX-minX)/2) (minY + (maxY-minY)/2)
  where
    Point2 maxX _ :+ _ = maximumBy (comparing (view (core.xCoord))) p
    Point2 minX _ :+ _ = minimumBy (comparing (view (core.xCoord))) p
    Point2 _ maxY :+ _ = maximumBy (comparing (view (core.yCoord))) p
    Point2 _ minY :+ _ = minimumBy (comparing (view (core.yCoord))) p

pAtCenter :: (Fractional r, Ord r) => SimplePolygon p r -> SimplePolygon p r
pAtCenter p = translateBy (neg $ toVec $ pCenter p) p
  where
    neg (Vector2 a b) = Vector2 (-a) (-b)


addPointAt :: forall p r. (Fractional r, Real r, Monoid p, Ord r) => Double -> SimplePolygon p r -> SimplePolygon p r
addPointAt t origin = worker (cir*realToFrac t) origin
  where
    -- worker :: Double -> SimplePolygon p r -> SimplePolygon p r
    worker d p
      | frac <= 0 = p
      | frac >= 1 = worker (d-len) (rotateRight 1 p)
      | otherwise =
          let (x:xs) = toPoints p
              x' = interpolate frac edge :+ mempty
          in rotateRight 1 $ fromPoints (x:x':xs)
      where
        edge = outerBoundaryEdge 0 p
        len = sqrt . realToFrac . sqSegmentLength $ edge
        frac = realToFrac (d / len)
    cir = CV.sum (CV.map approxLength (outerBoundaryEdges origin))
    approxLength = sqrt . realToFrac . sqSegmentLength

addPointsAt :: [Double] -> SimplePolygon p r -> SimplePolygon p r
addPointsAt = undefined

approxPointIndices :: SimplePolygon p r -> [Double]
approxPointIndices = undefined



------------------------------------------------------------------
-- Parameters

red :: PixelRGBA8
red = PixelRGBA8 0xFF 0x63 0x84 0xFF

grey :: PixelRGBA8
grey = PixelRGBA8 0xC7 0xC8 0xCD 0xFF

green :: PixelRGBA8
green = PixelRGBA8 0x4B 0xC0 0x4B 0XFF

black :: PixelRGBA8
black = PixelRGBA8 0x33 0x35 0x38 0xFF

white :: PixelRGBA8
white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF

bgColor :: String
bgColor = "white"

-- grey
polyColor :: PixelRGBA8
polyColor = grey

outlineColor :: PixelRGBA8
outlineColor = black

pathColor :: PixelRGBA8
pathColor = red

rootColor :: PixelRGBA8
rootColor = green

nodeColor :: PixelRGBA8
nodeColor = black -- PixelRGBA8 0xFF 0xFF 0xFF 0xFF -- polyColorP -- "white"

nodeRadius :: Double
nodeRadius = 0.2

------------------------------------------------------------------
-- Graphical utils

ppPolygonOutline :: Real r => PixelRGBA8 -> SimplePolygon p r -> SVG
ppPolygonOutline color p = withFillOpacity 0 $
  withStrokeColorPixel color $
  withStrokeLineJoin JoinRound $
  withStrokeWidth (defaultStrokeWidth*3) $ mkLinePathClosed
  [ (x, y)
  | Point2 x y <- map (fmap realToFrac . _core) $ toPoints p
  ]

ppPolygonBody :: Real r => PixelRGBA8 -> SimplePolygon p r -> SVG
ppPolygonBody color p = withFillOpacity 1 $
  withFillColorPixel color $
  withStrokeLineJoin JoinRound $
  mkLinePathClosed
  [ (x, y)
  | Point2 x y <- map (fmap realToFrac . _core) $ toPoints p
  ]

ppPolygonNodes :: (Real r) => PixelRGBA8 -> SimplePolygon p r -> SVG
ppPolygonNodes color p = mkGroup $
  map (ppPolygonNode color p) [0 .. size p-1]

ppPolygonNode :: (Real r) => PixelRGBA8 -> SimplePolygon p r -> Int -> SVG
ppPolygonNode color p idx =
  withFillColorPixel outlineColor $
  withStrokeColorPixel color $
  let Point2 x y = realToFrac <$> p^.outerVertex idx.core
  in translate x y $ mkCircle nodeRadius

ppSSSP :: (Real r, Fractional r, Ord r) => SimplePolygon p r -> SVG
ppSSSP p = ppSSSP' tree p'
  where
    p' = fromPoints $ toPoints p
    tree = sssp (triangulate p')

ppSSSP' :: Real r => SSSP -> SimplePolygon p r -> SVG
ppSSSP' tree p = mkGroup $
  map (ppPathTo tree p) [1 .. size p - 1]

ppLineSegment :: Real r => LineSegment 2 p r -> SVG
ppLineSegment (LineSegment' a b) = ppLine (a^.core) (b^.core)

ppLine :: Real r => Point 2 r -> Point 2 r -> SVG
ppLine a b =
    mkLine (x1,y1) (x2,y2)
  where
    Point2 x1 y1 = realToFrac <$> a
    Point2 x2 y2 = realToFrac <$> b

ppPathTo :: (Real r) => SSSP -> SimplePolygon p r -> Int -> SVG
ppPathTo t p nth = withFillOpacity 0 $ withStrokeColorPixel pathColor $ mkLinePath
    [ (x,y)
    | i <- reverse $ path nth
    , let Point2 x y = realToFrac <$> p^.outerVertex i.core
    ]
  where
    path 0   = [0]
    path idx = idx : path (t VU.! idx)

data T = T Int [T] deriving (Show)

ssspTree :: (Fractional r, Ord r) => SimplePolygon p r -> T
ssspTree p = worker 0
  where
    p' = fromPoints $ toPoints p
    t = sssp (triangulate p')
    worker idx = T idx [ worker n | n <- [0 .. size p-1], t VU.! n == idx, n /= idx]

morphSSSP :: (Fractional r, Ord r) => SimplePolygon p r -> SimplePolygon p r
morphSSSP p = pAtCenter $
    fromPoints
    [ case lookup idx pos of
        Nothing -> error $ "invalid position: " ++ show idx
        Just (x, y, rowLength) ->
          let leftMost = negate ((rowLength-1)/2)
          in Point2 (leftMost + fromIntegral x) (fromIntegral $ negate y) :+ (p^.outerVertex idx.extra)
    | idx <- [0 .. size p-1]
    ]
  where
    t = ssspTree p
    flat = flatten t
    pos =
      [ (elt, (x, y, fromIntegral $ length row))
      | (y, row) <- zip [0::Int ..] flat
      , (x, elt) <- zip [0::Int ..] row
      ]

flatten :: T -> [[Int]]
flatten (T n sub) = [n] : map concat (transpose (map flatten sub))

lerpPolygon :: forall p r. (Fractional r, Eq r) => Double -> SimplePolygon p r -> SimplePolygon p r -> SimplePolygon p r
lerpPolygon t a b = fromPoints $
  zipWith fn (toPoints b) (toPoints a)
  where
    fn :: Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p
    fn (a :+ p) (b :+ _) = lerpPoint t a b :+ p

lerpLineSegment :: Fractional r => Double -> LineSegment 2 p r -> LineSegment 2 p r -> LineSegment 2 p r
lerpLineSegment t (LineSegment' (a1 :+ p1) (b1 :+ p2)) (LineSegment' (a2 :+ _) (b2 :+ _)) =
  ClosedLineSegment (lerpPoint t a1 a2 :+ p1) (lerpPoint t b1 b2 :+ p2)


lerpPoint :: Fractional r => Double -> Point 2 r -> Point 2 r -> Point 2 r
lerpPoint t a b = Point $ lerp (realToFrac t) (toVec a) (toVec b)

lerpPoint' :: Fractional r => Double -> Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p
lerpPoint' t (a :+ p) (b :+ _) = lerpPoint t a b :+ p

lerpRectangle :: Fractional r => Double -> Rectangle p r -> Rectangle p r -> Rectangle p r
lerpRectangle t a b = box (lerpPoint' t (minPoint a) (minPoint b)) (lerpPoint' t (maxPoint a) (maxPoint b))

------------------------------------------------------------------
-- Random data

genPoint :: (RandomGen g, Random r, Fractional r) => Rand g (Point 2 r)
genPoint = Point2 <$> r <*> r
  where
    r = getRandomR (screenBottom, screenTop)

genPoints :: (RandomGen g, Random r, Fractional r) => Int -> Rand g [Point 2 r]
genPoints n = replicateM n genPoint

genLineSegment :: (RandomGen g, Random r, Fractional r) => Rand g (LineSegment 2 () r)
genLineSegment = do
  a <- genPoint
  b <- genPoint
  pure $ ClosedLineSegment (ext a) (ext b)
