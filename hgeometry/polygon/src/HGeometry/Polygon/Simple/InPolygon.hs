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
  -- , AboveCount(..)
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
q `inSimplePolygon` pg = case ifoldMapOf outerBoundaryWithNeighbours countAbove pg of
                            OnEdge s                       -> OnBoundaryEdge s
                            NumStrictlyAbove m | odd m     -> StrictlyInside
                                               | otherwise -> StrictlyOutside
  where
    -- we count the number of by the vertical upward ray from q intersects the boundary of
    -- the polygon. If the number of times we intersect the boundary is odd we are inside,
    -- and outside othwerise.
    --
    --
    -- Generally, countAbove will compute the contribution of the edge uv (which is edge i).
    --
    --
    -- we have to take special care of vertical edges, and when the ray goes through a
    -- vertex u.
    countAbove (i,_) (u,(p,v)) = case (u^.xCoord) `compare` (q^.xCoord) of
      LT | (q^.xCoord) < (v^.xCoord) -> belowLineSeg i u v
         -- for q to lie below the edge, v has to lie right of q and q has to actually lie
         -- below the line segment.
         --
         -- Note that if q lies strictly below v we don't count it here. We handle it
         -- when handing vertex v
         | otherwise                 -> mempty

      GT | (q^.xCoord) > (v^.xCoord) -> belowLineSeg i v u
            -- for q to lie below the edge, v has to lie left of q and
            -- q has to actually lie below the line through u and v.
         | otherwise                 -> mempty


      EQ -> case (u^.yCoord) `compare` (q^.yCoord) of
              EQ                             -> OnEdge i
                -- q == u, so it lies on edge i

              LT | (q^.xCoord) == (v^.xCoord) &&
                   (q^.yCoord) < (v^.yCoord) -> OnEdge i
                 -- q lies above u. So the only case in which q does lie on the edge uv
                 -- is if it is vertical, and q lies on it.
                 | otherwise                 -> mempty
                 -- q lies above u, so otherwise it does not lie on the edge starting at u.

              GT -> case (q^.xCoord) `compare` (v^.xCoord) of
                EQ | (q^.yCoord) > (v^.yCoord) -> OnEdge i
                   | otherwise                 -> mempty
                   -- the edge uv is vertical. We already established that u lies above
                   -- q, so it lies on the edge only if v lies strictly below q.

                LT | q^.xCoord <= p^.xCoord -> mempty
                   -- the predecessor vertex p and v lie on the same side of the vertical line
                   -- through u. So we don't count this vertex/edge
                   -- TODO: not sure if this should be <= or <
                   | otherwise              -> belowLineSeg i u v
                GT | q^.xCoord >= p^.xCoord -> mempty -- same as before v and p on the same side.
                   | otherwise              -> belowLineSeg i v u

    -- | count the edge if q is below the line through l and r,
    --
    -- pre: l is left of r.
    -- pre: q-x lies in the interval [lx,rx]
    belowLineSeg i l r = case ccw l q r of
      CW       -> mempty -- q lies strictly above the segment lr
      CoLinear -> OnEdge i
      CCW      -> NumStrictlyAbove 1  -- q lies strictly below the segment lr


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
                             Just _                                   -> False
                             Nothing                                  -> False
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
inCone q a l r = case cwCmpAroundWith ((l^.asPoint) .-. (a^.asPoint)) a (q^.asPoint) (r^.asPoint) of
                   GT -> False
                   _  -> True

  -- case ccw a l q of
  --                  CCW -> False
  --                  _   -> case ccw a r q of
  --                           CW -> False
  --                           _  -> True

-- it seems we cannot define the cmpCCwAroundwith with flip; since now the zero vector is actually treated last. i.e. they are not inverses.
