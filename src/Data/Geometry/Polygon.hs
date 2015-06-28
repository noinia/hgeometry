{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Polygon where


import           Control.Applicative
import           Control.Lens hiding (Simple, only)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Line
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Maybe(mapMaybe)
import           Data.Proxy
import           Data.Range

import qualified Data.CircularList as C

--------------------------------------------------------------------------------
-- * Polygons

-- | We distinguish between simple polygons (without holes) and Polygons with holes.
data PolygonType = Simple | Multi


data Polygon (t :: PolygonType) p r where
  SimplePolygon :: C.CList (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CList (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r

type SimplePolygon = Polygon Simple

type MultiPolygon  = Polygon Multi

-- | Polygons are per definition 2 dimensional
type instance Dimension (Polygon t p r) = 2
type instance NumType   (Polygon t p r) = r

-- * Functions on Polygons

outerBoundary :: forall t p r. Lens' (Polygon t p r) (C.CList (Point 2 r :+ p))
outerBoundary = lens get set
  where
    get                     :: Polygon t p r -> C.CList (Point 2 r :+ p)
    get (SimplePolygon vs)  = vs
    get (MultiPolygon vs _) = vs

    set                           :: Polygon t p r -> C.CList (Point 2 r :+ p) -> Polygon t p r
    set (SimplePolygon _)      vs = SimplePolygon vs
    set (MultiPolygon  _   hs) vs = MultiPolygon vs hs

holes :: forall p r. Lens' (Polygon Multi p r) [Polygon Simple p r]
holes = lens get set
  where
    get :: Polygon Multi p r -> [Polygon Simple p r]
    get (MultiPolygon _ hs) = hs
    set :: Polygon Multi p r -> [Polygon Simple p r] -> Polygon Multi p r
    set (MultiPolygon vs _) hs = MultiPolygon vs hs


-- | The vertices in the polygon. No guarantees are given on the order in which
-- they appear!
vertices :: Polygon t p r -> [Point 2 r :+ p]
vertices (SimplePolygon vs)   = C.toList vs
vertices (MultiPolygon vs hs) = C.toList vs ++ concatMap vertices hs



fromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
fromPoints = SimplePolygon . C.fromList

-- | The edges along the outer boundary of the polygon. The edges are half open.
outerBoundaryEdges :: Polygon t p r -> C.CList (LineSegment 2 p r)
outerBoundaryEdges = toEdges . (^.outerBoundary)


-- | Given the vertices of the polygon. Produce a list of edges. The edges are
-- half-open.
toEdges    :: C.CList (Point 2 r :+ p) -> C.CList (LineSegment 2 p r)
toEdges vs = let vs' = C.toList vs in
  C.fromList $ zipWith (\p q -> LineSegment (Closed p) (Open q)) vs' (tail vs' ++ vs')

-- | Check if a point lies strictly inside a polygon
inPolygon                             :: forall p r. (Fractional r, Ord r)
                                      => Point 2 r -> Polygon Simple p r -> Bool
q `inPolygon` pg@(SimplePolygon _)    = odd k
  where
    l = horizontalLine $ q^.yCoord

    -- Given a line segment, compute the intersection point (if a point) with the
    -- line l
    intersectionPoint = asA (Proxy :: Proxy (Point 2 r)) . (`intersect` l)

    -- Count the number of intersections that the horizontal line through q maxes
    -- with the polygon, that are strictly to the right of q. If this number is odd
    -- the point lies within the polygon.
    --
    --
    -- note that: - by the asA (Point 2 r) we ignore horizontal segments (as desired)
    --            - by the filtering, we effectively limit l to an open-half line, starting
    --               at the (open) point q.
    --            - by using half-open segments as edges we avoid double counting
    --               intersections that coincide with vertices.
    --
    -- See http://geomalgorithms.com/a03-_inclusion.html for more information.
    k = length
      . filter (\p -> p^.xCoord > q^.xCoord)
      . mapMaybe intersectionPoint . C.toList . outerBoundaryEdges $ pg


-- TODO: The code below would work for a multipolgyon, appart from the boundary case if the
--  point is on the boundary of a hole.
-- q `inPolygon` pg@(MultiPolygon vs hs) = and $ q `inPolygon` SimplePolygon vs
--                                             : [ not $ q `inPolygon` h
--                                               | h <- hs
--                                               ]

testQ = map (`inPolygon` testPoly) [ point2 1 1    -- True
                                   , point2 0 0    -- False
                                   , point2 5 14   -- True
                                   , point2 5 10   -- True
                                   , point2 10 5   -- False
                                   ]

testPoly :: SimplePolygon () Rational
testPoly = SimplePolygon . C.fromList . map only $ [ point2 0 0
                                                   , point2 10 0
                                                   , point2 10 10
                                                   , point2 5 15
                                                   , point2 1 11
                                                   ]
