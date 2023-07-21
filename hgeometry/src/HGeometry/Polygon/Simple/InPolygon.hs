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
  ( inPolygon
  -- , insidePolygon
  -- , onBoundary
  ) where

import Control.Lens
import HGeometry.Boundary
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Polygon.Simple.Class
import HGeometry.Polygon.Class
import HGeometry.Intersection

--------------------------------------------------------------------------------

{- $setup
>>> import HGeometry.Polygon.Simple
>>> :{
let simplePoly :: SimplePolygon (Point 2 Int)
    simplePoly = uncheckedFromCCWPoints $
                 [ Point2 0 0
                 , Point2 10 0
                 , Point2 10 10
                 , Point2 5 15
                 , Point2 1 11
                 ]
:}
-}

-- simpleTriangle :: SimplePolygon (Point 2 Int)
-- simpleTriangle = uncheckedFromCCWPoints $ [ Point2 0 0, Point2 2 0, Point2 1 1]
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
inPolygon        :: forall queryPoint simplePolygon point r.
                    ( Num r, Ord r, Point_ point 2 r, Point_ queryPoint 2 r
                    , SimplePolygon_ simplePolygon point r
                    )
                 => queryPoint -> simplePolygon
                 -> PointLocationResultWith (VertexIx simplePolygon)
q `inPolygon` pg = case ifoldMapOf outerBoundaryEdges countAbove pg of
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
    countAbove i (u,v) = case (u^.xCoord) `compare` (v^.xCoord) of
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
