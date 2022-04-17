{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
module Inflate (animateInflate) where

import Control.Lens                  ((&), (^.))
import Data.Ext                      (core, ext, (:+) ((:+)))
import Geometry.Ball            (Touching (Touching), pattern Circle)
import Geometry.Interval        ()
import Geometry.LineSegment     (LineSegment (OpenLineSegment), sqSegmentLength)
import Geometry.Point           (Point (Point2))
import Geometry.Polygon         (SimplePolygon, outerVertex, simpleFromPoints, size)
import Geometry.Polygon.Inflate (Arc (Arc), inflate)
import Geometry.Transformation
import Geometry.Vector          (pattern Vector2)
import Data.Intersection             (IsIntersectableWith (intersect),
                                      NoIntersection (NoIntersection))
import Data.RealNumber.Rational      (RealNumber)
import Data.Vinyl                    (Rec (RNil, (:&)))
import Data.Vinyl.CoRec              (Handler (H), match)
import Graphics.SvgTree              (LineJoin (..), Origin (..), PathCommand (..))
import Linear.V2                     (V2 (V2))
import Reanimate                     (Animation, adjustZ, animate, applyE, fadeOutE, mkPath,
                                      newSpriteSVG_, overEnding, pauseAtEnd, play, scene,
                                      setDuration, withFillColorPixel, withFillOpacity,
                                      withStrokeColorPixel, withStrokeLineJoin)

import Common (black, green, outlineColor, pAtCenter, ppPolygonOutline)

testPolygon1 :: SimplePolygon () (RealNumber 10)
testPolygon1 = scaleBy (Vector2 1 3) $ pAtCenter $ simpleFromPoints $ map ext
  [ Point2 0 0
  , Point2 1 1
  , Point2 3 1
  , Point2 3 2
  , Point2 (-3) 2
  , Point2 (-3) 1
  , Point2 (-1) 1
  ]

animateInflate :: Animation
animateInflate = scene $ do
  adjustZ succ $ newSpriteSVG_ $ ppPolygonOutline black testPolygon1
  play $ animate (\t ->
    let p = inflate t testPolygon1
    in withStrokeLineJoin JoinRound $
      withStrokeColorPixel outlineColor $
      withFillColorPixel green $ withFillOpacity 1 $ mkPath $ lowerArc p)
    & setDuration 5
    & pauseAtEnd 2
    & applyE (overEnding 1 fadeOutE)

lowerArc :: (Real r) => SimplePolygon (Arc r) r -> [PathCommand]
lowerArc p =
  MoveTo OriginAbsolute [V2 (realToFrac startX) (realToFrac  startY)] :
  concat
  [ if (this == edgeStart && next == edgeEnd) || this == next || minSqRadius == 0
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
      :& RNil
      )
  | i <- [0 .. size p-2]
  , let (this :+ Arc c (edgeStart, edgeEnd)) = p ^. outerVertex i
        edgeLine :: LineSegment 2 () Double
        edgeLine = realToFrac <$> OpenLineSegment (ext edgeStart) (ext edgeEnd)
        radiusThis = sqSegmentLength (OpenLineSegment (ext c) (ext this))
        radiusNext = sqSegmentLength (OpenLineSegment (ext c) (ext next))
        maxSqRadius = max radiusThis radiusNext
        minSqRadius = min radiusThis radiusNext
        radius = sqrt $ realToFrac maxSqRadius
        next = p ^. outerVertex (i+1) . core
        circle = Circle (ext $ realToFrac <$> c) radius
        Point2 dst_x dst_y = realToFrac <$> next
        arcTo x y = EllipticalArc OriginAbsolute
          [ (radius,radius, 0, False, True, V2 x y) ]
  ] ++ [EndPath]
  where
    Point2 startX startY = p ^. outerVertex 0 . core

