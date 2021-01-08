{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PartialTypeSignatures       #-}
module Main (main) where

import           Control.Lens
import qualified Data.Foldable             as F
import           Data.List                 (nub, transpose)
import           Data.Ord
import qualified Data.Vector               as V
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Circular.Util as CV
import qualified Data.Vector.Unboxed       as VU
import           Linear.V2                 (V2 (V2))
import           Linear.Vector             (Additive (lerp))
import           Reanimate
import           Reanimate.Svg
import           Reanimate
import           Graphics.SvgTree (PathCommand(..), Origin(..), Coord(..), RPoint(..), LineJoin(..))
import           Reanimate.Animation       (Sync (SyncFreeze))

import Algorithms.Geometry.SSSP
import Data.Ext
import Data.Geometry.Interval
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Ball
import Data.Geometry.Polygon
import Data.Geometry.Vector
import Data.Geometry.Polygon.Inflate ()
import Data.Geometry.Line
import Data.Intersection
import Data.Maybe
import Data.Vinyl
import Data.Vinyl.CoRec
import Data.RealNumber.Rational

import Common

import Debug.Trace

main :: IO ()
main = reanimate $
  mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
  scene $ do
    newSpriteSVG_ $ mkBackground bgColor
    play pseudo
    -- play $ ssspSingle
    -- play $ ssspMulti
    -- play $ ssspMorph
    -- wait 1

testPolygon1 :: SimplePolygon () (RealNumber 10)
testPolygon1 = pScaleV (Vector2 1 3) $ pAtCenter $ simpleFromPoints $ map ext 
  [ Point2 0 0
  , Point2 1 1
  , Point2 3 1
  , Point2 3 2
  , Point2 (-3) 2
  , Point2 (-3) 1
  , Point2 (-1) 1
  ]

pseudo :: Animation
pseudo = scene $ do
  adjustZ (succ) $ newSpriteSVG_ $ ppPolygonOutline black testPolygon1
  -- newSpriteSVG_ $ ppPolygonNodes testPolygon1
  play $ (animate $ \t ->
    let p = inflate t testPolygon1
    -- in ppPolygonOutline p
    in withStrokeLineJoin JoinRound $
      withStrokeColorPixel outlineColor $
      withFillColorPixel green $ withFillOpacity 1 $ mkPath $ lowerArc p)
    & setDuration 5
    & pauseAtEnd 2
    & applyE (overEnding 1 fadeOutE)
  -- wait 1



----------------------------------------------------
-- Implementation
type Inflated r = Point 2 r
data InflatedPoint r
  = Partial (Point 2 r)
  | Done
  deriving (Show)

data Arc r = Arc
  { arcCenter :: Point 2 r
  , arcEdge   :: (Point 2 r, Point 2 r)
  } deriving (Show)

type Parent = Int

markParents :: SSSP -> SimplePolygon p r -> SimplePolygon Parent r
markParents t p = unsafeFromCircularVector $
  CV.imap (\i (p :+ _) -> p :+ t VU.! i) (p^.outerBoundaryVector)

addPseudoPoints :: (Ord r, Fractional r) => SimplePolygon Parent r -> SimplePolygon Parent r
addPseudoPoints p = fromPoints $ concatMap worker [0 .. size p - 1]
  where
    worker nth = do
        pointA : catMaybes [ (:+ parent nth)     <$> getIntersection edge lineA
                           , (:+ parent (nth+1)) <$> getIntersection edge lineB ]
      where
        lookup idx = p ^. outerVertex idx
        pointA = lookup nth
        pointB = lookup (nth+1)
        parent idx = p^.outerVertex idx.extra
        lineA = lineThrough
          (lookup (parent nth) ^. core)
          (lookup (parent (parent nth)) ^. core)
        lineB = lineThrough
          (lookup (parent (nth+1)) ^. core)
          (lookup (parent (parent (nth+1))) ^. core)
        edge = OpenLineSegment pointA pointB
        getIntersection segment line =
          match (segment `intersect` line) (
               H (\NoIntersection -> Nothing)
            :& H (\pt -> Just pt)
            :& H (\LineSegment{} -> Nothing)
            :& RNil
          )

dropDups :: Eq a => CV.CircularVector (a :+ p) -> CV.CircularVector (a :+ p)
dropDups v = CV.unsafeFromVector $ CV.ifilter worker v
  where
    worker i a = _core a /= _core (CV.index v (i+1))

annotate :: (Real r, Fractional r, _) =>
  Double -> SimplePolygon Parent r -> SimplePolygon Parent r -> SimplePolygon (Arc r) r
annotate t original p = unsafeFromCircularVector $
    -- dropDups $
    CV.imap ann (p^.outerBoundaryVector)
  where
    n = size p
    nO = size original
    visibleDist = V.maximum distanceTreeSum * t
    parent idx = p^.outerVertex idx.extra
    parentO idx = original^.outerVertex idx.extra
    getLineO idx = OpenLineSegment (original ^. outerVertex (parentO idx)) (original ^. outerVertex idx)
    getLine idx = OpenLineSegment (original ^. outerVertex (parent idx)) (p ^. outerVertex idx)

    ann i a =
        ptLocation i :+ arc
      where
        start = p ^. outerVertex i . core
        end = p ^. outerVertex (i+1) . core
        startDone = ptLocation i == p ^. outerVertex i . core
        endDone = ptLocation (i+1) == p ^. outerVertex (i+1) . core
        arc = Arc
          { arcCenter = -- traceShow (i,commonParent original (parent i) (parent (i+1))) $
              if startDone && endDone
                then p ^. outerVertex i . core
                else original ^. outerVertex (commonParent original (parent i) (parent (i+1))) . core
          , arcEdge   = (start, end) }

    -- Array of locations for points in the original polygon.
    ptLocationsO = V.generate nO ptLocationO
    ptLocationO 0 = (original ^. outerVertex 0 . core)
    ptLocationO i
      | frac <= 0 = ptLocationsO V.! (parentO i)
      | frac >= 1 = (original ^. outerVertex i . core)
      | otherwise = (interpolate frac (getLineO i))
      where
        dParent = distanceTreeSum V.! parentO i
        dSelf   = oDistance VU.! i
        frac    = realToFrac ((visibleDist - dParent) / dSelf)

    -- Locations for original points and steiner points.
    ptLocation 0 = (p ^. outerVertex 0 . core)
    ptLocation i
      | frac <= 0 = ptLocationsO V.! (parent i)
      | frac >= 1 = (p ^. outerVertex i . core)
      | otherwise = (interpolate frac (getLine i))
      where
        dParent = distanceTreeSum V.! parent i
        dSelf   = sqrt $ realToFrac $ sqSegmentLength $ getLine i
        frac    = realToFrac ((visibleDist - dParent) / dSelf)
    
    oDistance = VU.generate nO $ \i ->
      case i of
        0 -> 0
        _ -> sqrt $ realToFrac $ sqSegmentLength $ getLineO i
    distanceTreeSum = V.generate nO $ \i ->
      case i of
        0 -> 0
        _ -> distanceTreeSum V.! parentO i + oDistance VU.! i



commonParent :: SimplePolygon Parent r -> Int -> Int -> Int
commonParent p a b = worker 0 (parents a) (parents b)
  where
    worker shared (x:xs) (y:ys)
      | x == y = worker x xs ys
    worker shared _ _ = shared
    parents 0 = [0]
    parents i = parents (p ^. outerVertex i . extra) ++ [i]

inflate :: (Real r, Fractional r, _) => Double -> SimplePolygon () r -> SimplePolygon (Arc r) r
inflate t p = annotate t marked pseudo
  where
    marked = markParents (sssp (triangulate p)) p
    pseudo = addPseudoPoints marked

-- i: 2
-- this: 3, 0
-- next: 3, 3
-- c:    1, 0
--
lowerArc :: (Real r, _) => SimplePolygon (Arc r) r -> [PathCommand]
lowerArc p = MoveTo OriginAbsolute [V2 (realToFrac x) (realToFrac  y)] : concat
  [ if (this == edgeStart && next == edgeEnd) || this == next || minRadius == 0
    then [LineTo OriginAbsolute [V2 dst_x dst_y]]
    else match (edgeLine `intersect` circle) (
        H (\NoIntersection -> [arcTo dst_x dst_y])
      :& H (\Touching{} -> [arcTo dst_x dst_y])
      :& H (\(Point2 x1 y1) ->
        if radiusThis <= radiusNext
          then [LineTo OriginAbsolute [V2 x1 y1], arcTo dst_x dst_y]
          else [arcTo x1 y1, LineTo OriginAbsolute [V2 dst_x dst_y]])
      :& H (\(Point2 x1 y1, Point2 x2 y2) ->
          [arcTo x1 y1, LineTo OriginAbsolute [V2 x2 y2], arcTo dst_x dst_y])
      -- :& H (\(Point2 x1 y1, Point2 x2 y2) ->
      --     [LineTo OriginAbsolute [V2 x1 y1, V2 x2 y2, V2 dst_x dst_y]])
      :& RNil
      )
  | i <- [0 .. size p-2]
  , let (this :+ Arc c (edgeStart, edgeEnd)) = p ^. outerVertex i
        edgeLine :: LineSegment 2 () Double
        edgeLine = realToFrac <$> OpenLineSegment (ext edgeStart) (ext edgeEnd)
        radiusThis = sqSegmentLength (OpenLineSegment (ext c) (ext this))
        radiusNext = sqSegmentLength (OpenLineSegment (ext c) (ext next))
        sqRadius = max radiusThis radiusNext
        minRadius = min radiusThis radiusNext
        radius = sqrt $ realToFrac sqRadius
        next = p ^. outerVertex (i+1) . core
        circle :: Circle () Double
        circle = Circle (ext $ realToFrac <$> c) (radius)
        Point2 dst_x dst_y = realToFrac <$> next
        entireArc = EllipticalArc OriginAbsolute
          [ (radius,radius, 0, False, True, V2 dst_x dst_y) ]
        arcTo x y = EllipticalArc OriginAbsolute
          [ (radius,radius, 0, False, True, V2 x y) ]
  -- , traceShow (this, next, edgeStart, edgeEnd, radiusThis, radiusNext) True
  ] ++ [EndPath]
  where
    Point2 x y = p ^. outerVertex 0 . core
  
