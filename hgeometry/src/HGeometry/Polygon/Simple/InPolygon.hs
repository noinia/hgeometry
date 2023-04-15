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
import HGeometry.Line
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon.Simple.Class
import HGeometry.Polygon.Class
import HGeometry.Properties
import HGeometry.Intersection
import Hiraffe.Graph

import HGeometry.Polygon.Simple

--------------------------------------------------------------------------------

{- $setup
>>> import Control.Lens.Extras
>>> :{
-- import qualified Data.Vector.Circular as CV
let simplePoly :: SimplePolygon () (RealNumber 10)
    simplePoly = fromPoints . map ext $
      [ Point2 0 0
      , Point2 10 0
      , Point2 10 10
      , Point2 5 15
      , Point2 1 11
      ]
    simpleTriangle :: SimplePolygon () (RealNumber 10)
    simpleTriangle = fromPoints  . map ext $
      [ Point2 0 0, Point2 2 0, Point2 1 1]
:} -}

simplePoly :: SimplePolygon (Point 2 Int)
simplePoly = uncheckedFromCCWPoints $
             [ Point2 0 0
             , Point2 10 0
             , Point2 10 10
             , Point2 5 15
             , Point2 1 11
             ]

simpleTriangle :: SimplePolygon (Point 2 Int)
simpleTriangle = uncheckedFromCCWPoints $ [ Point2 0 0, Point2 2 0, Point2 1 1]


test = Point2 1 1 `inPolygon` simplePoly

data AboveCount seg = OnEdge !seg
                    | NumStrictlyAbove {-# UNPACK #-} !Int
                    deriving (Show,Eq)

instance Semigroup (AboveCount seg) where
  l@(OnEdge _)         <> _                    = l -- ^ prefers the first segment
  _                    <> r@(OnEdge _)         = r -- ^ already found a boundary
  (NumStrictlyAbove l) <> (NumStrictlyAbove r) = NumStrictlyAbove (l+r)

instance Monoid (AboveCount seg) where
  mempty = NumStrictlyAbove 0

-- | Returns true if the point lies in the polygon

-- | Check if a point lies inside a polygon, on the boundary, or outside of the polygon.
-- Running time: O(n).
--
-- >>> Point2 1 1 `inPolygon` simplePoly
-- StrictlyInside
-- >>> Point2 0 0 `inPolygon` simplePoly
-- OnBoundaryEdge 0
-- >>> Point2 10 0 `inPolygon` simplePoly
-- StrictlyOnBoundary
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
                  => queryPoint -> simplePolygon -> PointLocationResultWith (VertexIx simplePolygon)
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
                           LT                      -> countAbove' i u v
                           GT                      -> countAbove' i v u
    -- count the edge if q
    countAbove' i l r
      | (Point1 $ q^.xCoord) `intersects` (Interval (ClosedE' $ l^.xCoord) (OpenE' $ r^.xCoord))
                  = case ccw (l^.asPoint) (q^.asPoint) (r^.asPoint) of
                      CW       -> NumStrictlyAbove 1  -- q lies strictly below the segment lr
                      CoLinear -> OnEdge i
                      CCW      -> mempty -- q lies strictly above the segment lr
      | otherwise = mempty
    -- test if q lies on the vertical edge defined by u and v
    onVerticalEdge u v = let yr = buildClosedInterval @(ClosedInterval r) (u^.yCoord) (v^.yCoord)
                         in q^.xCoord == u^.xCoord && (Point1 $ q^.yCoord) `intersects` yr










{-















-- | \( O(n) \) Test if q lies on the boundary of the polygon.
--
-- >>> Point2 1 1 `onBoundary` simplePoly
-- False
-- >>> Point2 0 0 `onBoundary` simplePoly
-- True
-- >>> Point2 10 0 `onBoundary` simplePoly
-- True
-- >>> Point2 5 13 `onBoundary` simplePoly
-- False
-- >>> Point2 5 10 `onBoundary` simplePoly
-- False
-- >>> Point2 10 5 `onBoundary` simplePoly
-- True
-- >>> Point2 20 5 `onBoundary` simplePoly
-- False
onBoundary                          :: ( Num r, Ord r
                                       , Vertex polygon ~ point
                                       , HasOuterBoundary polygon
                                       , Point_ point 2 r
                                       , Point_ queryPoint 2 r
                                       )
                                    => queryPoint -> polygon -> bool
(view asPoint -> q) `onBoundary` pg = anyOf outerBoundaryEdges (q `intersects`) pg

-- FIXME: inPolygon shouldn't require a fractional constraint; just Num r
--
-- new main idea just shoot the ray upward; we can then evaluate if p lies below or above
-- the edges.



inPolygon             :: ( Fractional r, Ord r
                         , SimplePolygon_ simplePolygon point
                         , Point_ queryPoint 2 r
                         )
                      => queryPoint -> simplePolygon -> PointLocationResult
(view asPoint q) `inPolygon` pg
  | q `onBoundary` pg = OnBoundary
  | otherwise         = q `inPolygon'` pg

-- | Returns true if the point lies in the polygon
-- pre: point lies inside or outside the polygon, not on its boundary.
inPolygon'        :: ( Fractional r, Ord r, Point_ point 2 r
                     , SimplePolygon_ simplePolygon point
                     )
                  => Point 2 r  -> simplePolygon -> PointLocationResult
q `inPolygon'` pg = if odd . length . mapMaybe intersectionPoint $ ups <> downs
                    then Inside else Outside
  where
    -- we don't care about horizontal edges
    (ups',_horizontals,downs') = partitionEdges $ pg^..outerBoundaryEdges
    partitionEdges = List.partition3 $ \s -> (s^.end.yCoord) `compare` (s^.start.yCoord)

    -- upward edges include start, exclude end
    ups   = map (\(LineSegment' a b) -> LineSegment (Closed a) (Open b)) ups'
    -- downward edges exclude start, include end
    downs = map (\(LineSegment' a b) -> LineSegment (Open a) (Closed b)) downs'

    -- Given an edge, compute the intersection point (if a point) with
    -- the line through the query point, and test if it lies strictly
    -- right of q.
    --
    -- See http://geomalgorithms.com/a03-_inclusion.html for more information.
    intersectionPoint s = case s `intersect` l of
                            undefined --- TODO; implement line x linesegment intersection
      -- =  F.find (\p -> p^.xCoord > q^.xCoord) . asA @(Point 2 r) . (`intersect` l)
    l = horizontalLine $ q^.yCoord


-- | Test if a point lies strictly inside the polgyon.
insidePolygon        :: ( Fractional r, Ord r
                        , Point_ queryPoint 2 r
                        , Point_ point 2 r
                        , SimplePolygon_ simplePolygon point
                        )
                     => queryPoint -> simplePolygon -> Bool
q `insidePolygon` pg = q `inPolygon` pg == Inside


-- testQ = map (`inPolygon` testPoly) [ Point2 1 1    -- Inside
--                                    , Point2 0 0    -- OnBoundary
--                                    , Point2 5 14   -- Inside
--                                    , Point2 5 10   -- Inside
--                                    , Point2 10 5   -- OnBoundary
--                                    , Point2 20 5   -- Outside
--                                    ]

-- testPoly :: SimplePolygon () Rational
-- testPoly = fromPoints . map ext $ [ Point2 0 0
--                                                   , Point2 10 0
--                                                   , Point2 10 10
--                                                   , Point2 5 15
--                                                   , Point2 1 11
--                                                   ]
-}
