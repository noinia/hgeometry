{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Implementation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Implementation
  ( ConvexPolygon
  , ConvexPolygonF
  , fromSimplePolygon, toSimplePolygon
  , _ConvexPolygon
  , isStrictlyConvex, isConvex
  , verifyConvex
  , maxInDirection
  , findMaxWith
  , inConvex
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import HGeometry.Boundary
import HGeometry.Box
import HGeometry.Cyclic
import HGeometry.Foldable.Util
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Convex.Class
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Simple.Implementation
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point
                -- ^ Convert to a simple polygon, i.e. forget the polygon is convex.
                }
  deriving newtype (NFData, Eq)

-- | By default we use a cyclic non-empty vector to represent convex polygons.
type ConvexPolygon :: Type -> Type
type ConvexPolygon = ConvexPolygonF (Cyclic NonEmptyVector)

-- | ConvexPolygons are isomorphic to SimplePolygons with the added
--   constraint that all vertices are strictly convex.
--
-- Note that this is unchecked; i.e. one can turn an arbitrary simple polygon
-- into a suposedly convex one.
_UncheckedConvexPolygon :: Iso (ConvexPolygonF f point) (ConvexPolygonF f' point')
                               (SimplePolygonF f point) (SimplePolygonF f' point')
_UncheckedConvexPolygon = iso toSimplePolygon ConvexPolygon

-- | Prism that can forget that the polygon is convex
--
_ConvexPolygon :: forall f point r. (Num r, Ord r, Point_ point 2 r
                                    , VertexContainer f point
               ) => Prism' (SimplePolygonF f point) (ConvexPolygonF f point)
_ConvexPolygon = prism' toSimplePolygon fromSimplePolygon

-- deriving instance Eq (ConvexPolygonF f point r)
-- | Polygons are per definition 2 dimensional
type instance Dimension (ConvexPolygonF f point) = 2
type instance NumType   (ConvexPolygonF f point) = NumType point

instance (ShiftedEq (f point), ElemCyclic (f point) ~ point
         ) => ShiftedEq (ConvexPolygonF f point) where
  type ElemCyclic (ConvexPolygonF f point) = point
  isShiftOf p q = isShiftOf (p^._UncheckedConvexPolygon) (q^._UncheckedConvexPolygon)

instance ( HasVertices (SimplePolygonF f point) (SimplePolygonF f point')
         ) => HasVertices (ConvexPolygonF f point) (ConvexPolygonF f point') where
  vertices = _UncheckedConvexPolygon . vertices

instance ( VertexContainer f point
         ) => HasPoints (ConvexPolygonF f point) (ConvexPolygonF f point') point point' where
  allPoints = _UncheckedConvexPolygon . allPoints

instance HasVertices' (SimplePolygonF f point) => HasVertices' (ConvexPolygonF f point) where
  type Vertex   (ConvexPolygonF f point) = Vertex   (SimplePolygonF f point)
  type VertexIx (ConvexPolygonF f point) = VertexIx (SimplePolygonF f point)
  vertexAt i = _UncheckedConvexPolygon . vertexAt i
  numVertices = numVertices . view _UncheckedConvexPolygon

instance ( HasOuterBoundary (SimplePolygonF f point)
         , VertexIx (SimplePolygonF f point) ~ Int
         ) =>
         HasOuterBoundary (ConvexPolygonF f point) where
  outerBoundary = _UncheckedConvexPolygon . outerBoundary
  outerBoundaryVertexAt i = _UncheckedConvexPolygon . outerBoundaryVertexAt i
  ccwOuterBoundaryFrom i = _UncheckedConvexPolygon.ccwOuterBoundaryFrom i
  cwOuterBoundaryFrom i = _UncheckedConvexPolygon.cwOuterBoundaryFrom i

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Point_ point 2 r
         ) => Polygon_ (ConvexPolygonF f point) point r where
  area = areaSimplePolygon
  extremes u p = (maxInDirection ((-1) *^ u) p, maxInDirection u p)
  ccwPredecessorOf u = _UncheckedConvexPolygon.ccwPredecessorOf u
  ccwSuccessorOf   u = _UncheckedConvexPolygon.ccwSuccessorOf u


instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Point_ point 2 r
         ) => SimplePolygon_ (ConvexPolygonF f point) point r where
  type ConstructableSimplePolygon (ConvexPolygonF f point) point r =
    ( VertexContainer f point
    , Ord r
    , Num r
    )
  -- | Additional precondition: the points actually form a convex polygon
  uncheckedFromCCWPoints = ConvexPolygon . uncheckedFromCCWPoints
  fromPoints pts = fromPoints pts >>= fromSimplePolygon

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Point_ point 2 r
         ) => ConvexPolygon_ (ConvexPolygonF f point) point r where


-- | Smart constructor to construct a strictly convex polygon from a
-- simple polygon.
fromSimplePolygon :: (Ord r, Num r, Point_ point 2 r, VertexContainer f point)
                  => SimplePolygonF f point -> Maybe (ConvexPolygonF f point)
fromSimplePolygon pg
  | isStrictlyConvex pg = Just (ConvexPolygon pg)
  | otherwise           = Nothing


instance ( Show point
         , SimplePolygon_ (ConvexPolygonF f point) point r
         ) => Show (ConvexPolygonF f point) where
  showsPrec = showsPrecSimplePolygon "ConvexPolygon"

instance ( Read point
         , SimplePolygon_ (ConvexPolygonF f point) point r
         ) => Read (ConvexPolygonF f point) where
  readsPrec = readsPrecSimplePolygon "ConvexPolygon"





{-
instance ( SimplePolygon_ (ConvexPolygonF f point) point r
         , SimplePolygon_ (SimplePolygonF f point) point r
         , Ord r, Fractional r)
       => HasSquaredEuclideanDistance (ConvexPolygonF f point) where
  pointClosestToWithDistance q = pointClosestToWithDistance q . toSimplePolygon
  -- TODO: we should be able to implement this in O(log n) time instead!!
-}

instance ( VertexContainer f point
         , DefaultTransformByConstraints (ConvexPolygonF f point) 2 r
         , Point_ point 2 r
         ) => IsTransformable (ConvexPolygonF f point)

instance ( VertexContainer f point
         , Point_ point 2 r, Num r, HasFromFoldable1 f
         ) => IsBoxable (ConvexPolygonF f point) where
  boundingBox pg = Box (Point2 xMin yMin) (Point2 xMax yMax)
    where
      xMin = view xCoord $ maxInDirection (Vector2 (-1) 0   ) pg
      xMax = view xCoord $ maxInDirection (Vector2 1    0   ) pg
      yMin = view yCoord $ maxInDirection (Vector2 0    (-1)) pg
      yMax = view yCoord $ maxInDirection (Vector2 0    1   ) pg

--------------------------------------------------------------------------------


-- | Verify that a convex polygon is strictly convex.
--
-- running time \( O(n) \)
verifyConvex :: (Ord r, Num r, Point_ point 2 r, VertexContainer f point)
              => ConvexPolygonF f point -> Bool
verifyConvex = isStrictlyConvex . toSimplePolygon

-- | \( O(n) \) Check if a polygon is strictly convex.
isStrictlyConvex :: (Ord r, Num r, Point_ point 2 r, VertexContainer f point)
                 => SimplePolygonF f point -> Bool
isStrictlyConvex = allOf outerBoundaryWithNeighbours isStrictlyConvexVertex
  where
    isStrictlyConvexVertex (v,(u,w)) = ccw u v w == CCW

-- | \( O(n) \) Check if a polygon is convex.
isConvex   :: (Ord r, Num r, Point_ point 2 r, VertexContainer f point)
           => SimplePolygonF f point -> Bool
isConvex = allOf outerBoundaryWithNeighbours isConvexVertex
  where
    isConvexVertex (v,(u,w)) = ccw u v w /= CW


--------------------------------------------------------------------------------

-- | Finds the extreme maximum point in the given direction. Based on
-- http://geomalgorithms.com/a14-_extreme_pts.html
--
--
-- pre: The input polygon is strictly convex.
--
-- running time: \(O(\log n)\)
maxInDirection   :: (Num r, Ord r, ConvexPolygon_ convexPolygon point r)
                 => Vector 2 r -> convexPolygon -> point
maxInDirection u = findMaxWith (cmpInDirection u)

-- | Find the maximum vertex in a convex polygon using a binary search.
-- \( O(\log n) \)
findMaxWith        :: (ConvexPolygon_ convexPolygon point r)
                   => (point -> point -> Ordering)
                   -> convexPolygon -> point
findMaxWith cmp pg = pg^.outerBoundaryVertexAt (worker 0 n)
  where
    n = numVertices pg
    a `icmp` b = (pg^.outerBoundaryVertexAt a) `cmp` (pg^.outerBoundaryVertexAt b)
    worker a b
      | localMaximum c = c
      | a+1==b         = b
      | otherwise      =
        case  (isUpwards a, isUpwards c, c `icmp` a /= LT) of
          (True, False, _)      -> worker a c -- A is up, C is down, pick [a,c]
          (True, True, True)    -> worker c b -- A is up, C is up, C is GTE A, pick [c,b]
          (True, True, False)   -> worker a c -- A is up, C is LT A, pick [a,c]
          (False, True, _)      -> worker c b -- A is down, C is up, pick [c,b]
          (False, False, False) -> worker c b -- A is down, C is down, C is LT A, pick [c,b]
          (False, _, True)      -> worker a c -- A is down, C is GTE A, pick [a,c]
      where
        c = (a+b) `div` 2
        localMaximum idx = idx `icmp` (c-1) == GT && idx `icmp` (c+1) == GT
    isUpwards idx = idx `icmp` (idx+1) /= GT


--------------------------------------------------------------------------------
-- * inConvex

-- 1. Check if p is on left edge or right edge.
-- 2. Do binary search:
--       Find the largest n where p is on the right of 0 to n.
-- 3. Check if p is on segment n,n+1
-- 4. Check if p is in triangle 0,n,n+1

-- | Check if a point lies inside a convex polygon, on the boundary, or outside of the
--   convex polygon.
--
-- \( O(\log n) \)
inConvex :: ( ConvexPolygon_ convexPolygon point r
            , Point_ queryPoint 2 r, Num r, Ord r)
         => queryPoint -> convexPolygon
         -> PointLocationResultWith (VertexIx convexPolygon)
inConvex (view asPoint -> q) poly
  | q `intersects` leftEdge  = OnBoundaryEdge n
  | q `intersects` rightEdge = OnBoundaryEdge 0
  | otherwise                = worker 1 n
  where
    n         = numVertices poly - 1
    point0    = point 0
    leftEdge  = ClosedLineSegment point0 (point n)
    rightEdge = ClosedLineSegment point0 (point 1)
    worker a b
      | a+1 == b                        =
        if q `onSegment` ClosedLineSegment (point a) (point b)
          then OnBoundaryEdge a
          else
            if q `intersects` Triangle point0 (point a) (point b)
              then StrictlyInside
              else StrictlyOutside
      | ccw point0 (point c) q == CCW = worker c b
      | otherwise                     = worker a c
      where c = (a+b) `div` 2

    point x = poly^.outerBoundaryVertexAt x.asPoint

instance ConvexPolygon_ (ConvexPolygonF f point) point r
         => HasInPolygon (ConvexPolygonF f point) point r where
  inPolygon = inConvex
