{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.InPolygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Testing if a point lies in a polygon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.InPolygon
  ( HasInPolygon(..)
  , inSimplePolygon
  -- , insidePolygon
  -- , onBoundary
  , containedIn
  ) where

import Control.Lens
import HGeometry.Boundary
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple.Class
import HGeometry.Properties

--------------------------------------------------------------------------------

{- $setup
>>> import HGeometry.Polygon.Simple
>>> import qualified Data.List.NonEmpty as NonEmpty
>>> :{
let simplePoly :: SimplePolygon (Point 2 Int)
    simplePoly = uncheckedFromCCWPoints . NonEmpty.fromList $
                 [ Point2 0 0
                 , Point2 10 0
                 , Point2 10 10
                 , Point2 5 15
                 , Point2 1 11
                 ]
:}
-}

-- simpleTriangle :: SimplePolygon (Point 2 Int)
-- simpleTriangle = uncheckedFromCCWPoints . NonEmpty.fromList $ [ Point2 0 0, Point2 2 0, Point2 1 1]
-- test = Point2 1 1 `inPolygon` simplePoly

data AboveCount seg = OnEdge !seg
                    | NumStrictlyAbove {-# UNPACK #-} !Int
                    deriving (Show,Eq)

instance Semigroup (AboveCount seg) where
  l@(OnEdge _)         <> _                    = l -- ^ prefers the first segment
  _                    <> r@(OnEdge _)         = r -- ^ already found a boundary
  (NumStrictlyAbove l) <> (NumStrictlyAbove r) = NumStrictlyAbove (l+r)

instance Monoid (AboveCount seg) where
  mempty = NumStrictlyAbove 0

-- | Types that implement a point-in-polygon test.
class HasInPolygon polygon point r | polygon -> point, point -> r where
  -- | Check if a point lies inside a polygon, on the boundary, or outside of the
  -- polygon. If the point lies on the boundary we also return an edge e containing the
  -- point (identified by the vertex v so that e follows v in the CCW order along the
  -- boundary). No guarantees are given about which edge is returned if the query point
  -- lies on multiple edges (i.e. when it coincides with a vertex.)
  inPolygon :: ( Num r, Ord r, Point_ queryPoint 2 r)
            => queryPoint -> polygon -> PointLocationResultWith (VertexIx polygon)
  default inPolygon :: ( Num r, Ord r, Point_ point 2 r, Point_ queryPoint 2 r
                       , SimplePolygon_ polygon point r
                       )
                    => queryPoint -> polygon
                    -> PointLocationResultWith (VertexIx polygon)
  inPolygon = inSimplePolygon

-- rename the thing below to InSimplePolygon?

-- | Check if a point lies inside a polygon, on the boundary, or
-- outside of the polygon. If the point lies on the boundary we also
-- return an edge e containing the point (identified by the vertex v
-- so that e follows v in the CCW order along the boundary). No
-- guarantees are given about which edge is returned if the query
-- point lies on multiple edges (i.e. when it coincides with a
-- vertex.)
--
--
-- Running time: O(n).
--
-- >>> Point2 1 1 `inPolygon` simplePoly
-- StrictlyInside
-- >>> Point2 0 0 `inPolygon` simplePoly
-- OnBoundaryEdge 0
-- >>> Point2 10 0 `inPolygon` simplePoly
-- OnBoundaryEdge 1
-- >>> Point2 5 13 `inPolygon` simplePoly
-- StrictlyInside
-- >>> Point2 5 10 `inPolygon` simplePoly
-- StrictlyInside
-- >>> Point2 10 5 `inPolygon` simplePoly
-- OnBoundaryEdge 1
-- >>> Point2 20 5 `inPolygon` simplePoly
-- StrictlyOutside
inSimplePolygon        :: forall queryPoint simplePolygon point r.
                          ( Num r, Ord r, Point_ point 2 r, Point_ queryPoint 2 r
                          , SimplePolygon_ simplePolygon point r
                          )
                       => queryPoint -> simplePolygon
                       -> PointLocationResultWith (VertexIx simplePolygon)
q `inSimplePolygon` pg = case ifoldMapOf outerBoundaryEdges countAbove pg of
                     OnEdge s                       -> OnBoundaryEdge s
                     NumStrictlyAbove m | odd m     -> StrictlyInside
                                        | otherwise -> StrictlyOutside
  -- we count the number m of non-vertical edges half-open [ell,r) that lie
  -- strictly above q. The idea is that q lies inside the polygon iff m is odd.
  --
  -- we have to be a bit careful in the case q lies on an edge
  -- (e.g. the <=> does not hold in that case). If we discover q lies
  -- on an edge e, we actually report that.
  where
    countAbove (i,_) (u,v) = case (u^.xCoord) `compare` (v^.xCoord) of
                               EQ | onVerticalEdge u v -> OnEdge i
                                  | otherwise          -> mempty
                               LT                      -> countAbove' i (AnClosedE u) (AnOpenE v)
                               GT                      -> countAbove' i (AnOpenE v)   (AnClosedE u)
    -- count the edge if q
    countAbove' i l r
      | (Point1 $ q^.xCoord) `intersects` (view xCoord <$> Interval l r)
                  = case ccw (l^._endPoint.asPoint) (q^.asPoint) (r^._endPoint.asPoint) of
                      CW       -> NumStrictlyAbove 1  -- q lies strictly below the segment lr
                      CoLinear -> OnEdge i
                      CCW      -> mempty -- q lies strictly above the segment lr
      | otherwise = mempty

    --- fixme, assign open and closed before; the issue seems to be that a rightmost vertex (local max) is assigned the open endpoint on both times. That essentialy treats is as lying outsie the pg.


    -- test if q lies on the vertical edge defined by u and v
    onVerticalEdge u v = let yr = buildClosedInterval @(ClosedInterval r) (u^.yCoord) (v^.yCoord)
                         in q^.xCoord == u^.xCoord && (Point1 $ q^.yCoord) `intersects` yr



--------------------------------------------------------------------------------
-- * Test if a segment is contained in a polygon

-- | test if the given line segment is contained in the polygon. It is also ok if the
-- segment lies partially on the boundary
containedIn :: ( ClosedLineSegment_ lineSegment point
               , SimplePolygon_ simplePolygon vertex r
               , Intersection (ClosedLineSegment vertex) lineSegment
                 ~ Maybe (LineSegmentLineSegmentIntersection lineSegment')
               , IsIntersectableWith (ClosedLineSegment vertex) lineSegment
               , NumType lineSegment' ~ r
               , HasInPolygon simplePolygon vertex r
               , Point_ point 2 r, Point_ vertex 2 r, Ord r, Fractional r
               ) => lineSegment -> simplePolygon -> Bool
containedIn seg poly = case (seg^.start) `inPolygon` poly of
    StrictlyInside    -> case (seg^.end) `inPolygon` poly of
                           StrictlyInside    -> not properIntersection
                           StrictlyOutside   -> False
                           OnBoundaryEdge vj -> not properIntersection && inCone' vj (seg^.start)
    StrictlyOutside   -> False
    OnBoundaryEdge vi -> case (seg^.end) `inPolygon` poly of
                           StrictlyInside    -> not properIntersection
                           StrictlyOutside   -> False
                           OnBoundaryEdge vj -> not properIntersection
                                                && inCone' vi (seg^.end)
                                                && inCone' vj (seg^.start)
  where
    properIntersection = anyOf outerBoundaryEdgeSegments (\edgeSeg ->
                           case edgeSeg `intersect` seg of
                             Just (LineSegment_x_LineSegment_Point p) -> not $
                               p /= (edgeSeg^.start.asPoint) || p /= (edgeSeg^.end.asPoint)
                             _                                        -> False
                                                         ) poly
    inCone' i q = let a = poly^?!vertexAt i
                      p = poly^?!vertexAt (i-1)
                      n = poly^?!vertexAt (i+1)
                  in inCone q a p n

-- | Test if a point lies inside a cone.
inCone           :: ( Point_ queryPoint 2 r, Point_ apex 2 r, Point_ point 2 r, Point_ point' 2 r
                    , Ord r, Num r
                    ) =>
                    queryPoint -> apex -> point -> point' -> Bool
inCone q a l r = case ccw a l q of
                   CCW -> False
                   _   -> case ccw a r q of
                            CW -> False
                            _  -> True
