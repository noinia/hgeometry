{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Data.Geometry.Polygon where


import           Control.Applicative
import           Control.Lens hiding (Simple, only)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Boundary
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Line
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Maybe(mapMaybe)
import           Data.Proxy
import           Data.Range
import           Frames.CoRec(asA)
import qualified Data.CircularList as C

--------------------------------------------------------------------------------
-- * Polygons

{- $setup
>>> :{
let simplePoly :: SimplePolygon () Rational
    simplePoly = SimplePolygon . C.fromList . map only $ [ point2 0 0
                                                         , point2 10 0
                                                         , point2 10 10
                                                         , point2 5 15
                                                         , point2 1 11
                                                         ]
:} -}

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


-- | Get all holes in a polygon
holeList                     :: Polygon t p r -> [Polygon Simple p r]
holeList (SimplePolygon _)   = []
holeList (MultiPolygon _ hs) = hs


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


-- | Test if q lies on the boundary of the polygon. Running time: O(n)
--
-- >>> point2 1 1 `onBoundary` simplePoly
-- False
-- >>> point2 0 0 `onBoundary` simplePoly
-- True
-- >>> point2 10 0 `onBoundary` simplePoly
-- True
-- >>> point2 5 13 `onBoundary` simplePoly
-- False
-- >>> point2 5 10 `onBoundary` simplePoly
-- False
-- >>> point2 10 5 `onBoundary` simplePoly
-- True
-- >>> point2 20 5 `onBoundary` simplePoly
-- False
--
-- TODO: testcases multipolygon
onBoundary        :: (Fractional r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `onBoundary` pg = any (q `onSegment`) es
  where
    out = SimplePolygon $ pg^.outerBoundary
    es = concatMap (C.toList . outerBoundaryEdges) $ out : holeList pg

-- | Check if a point lies inside a polygon, on the boundary, or outside of the polygon.
-- Running time: O(n).
--
-- >>> point2 1 1 `inPolygon` simplePoly
-- Inside
-- >>> point2 0 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 10 0 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 5 13 `inPolygon` simplePoly
-- Inside
-- >>> point2 5 10 `inPolygon` simplePoly
-- Inside
-- >>> point2 10 5 `inPolygon` simplePoly
-- OnBoundary
-- >>> point2 20 5 `inPolygon` simplePoly
-- Outside
--
-- TODO: Add some testcases with multiPolygons
inPolygon                                :: forall t p r. (Fractional r, Ord r)
                                         => Point 2 r -> Polygon t p r
                                         -> PointLocationResult
q `inPolygon` pg
    | q `onBoundary` pg                  = OnBoundary
    | odd k && not (any (q `inHole`) hs) = Inside
    | otherwise                          = Outside
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

    -- For multi polygons we have to test if we do not lie in a hole .
    inHole = insidePolygon
    hs     = holeList pg

-- | Test if a point lies strictly inside the polgyon.
insidePolygon        :: (Fractional r, Ord r) => Point 2 r -> Polygon t p r -> Bool
q `insidePolygon` pg = q `inPolygon` pg == Inside


testQ = map (`inPolygon` testPoly) [ point2 1 1    -- Inside
                                   , point2 0 0    -- OnBoundary
                                   , point2 5 14   -- Inside
                                   , point2 5 10   -- Inside
                                   , point2 10 5   -- OnBoundary
                                   , point2 20 5   -- Outside
                                   ]

testPoly :: SimplePolygon () Rational
testPoly = SimplePolygon . C.fromList . map only $ [ point2 0 0
                                                   , point2 10 0
                                                   , point2 10 10
                                                   , point2 5 15
                                                   , point2 1 11
                                                   ]
