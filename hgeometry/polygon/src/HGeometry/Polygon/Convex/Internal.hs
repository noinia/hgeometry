{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Internal
  ( ConvexPolygon
  , ConvexPolygonF(..)
  , fromSimplePolygon
  , _ConvexPolygon
  , _UncheckedConvexPolygon
  , isStrictlyConvex, isConvex
  , verifyConvex
  , maxInDirection
  , findMaxWith
  , inConvex
  , HalfPlaneConvexPolygonIntersection
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens hiding (holes)
import           Data.Bifunctor
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Vector.NonEmpty (NonEmptyVector)
import           HGeometry.Boundary
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.LineSegment.PossiblyDegenerate
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Triangle
import qualified HGeometry.Triangle as Triangle
import           HGeometry.Vector
import           HGeometry.Vector.NonEmpty.Util ()
import           Data.Functor.Classes

--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point
                -- ^ Convert to a simple polygon, i.e. forget the polygon is convex.
                }
  deriving newtype (NFData, Eq, Functor, Foldable, Foldable1, Eq1)
  deriving stock (Traversable)


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


instance Traversable1 f => Traversable1 (ConvexPolygonF f) where
  traverse1 f (ConvexPolygon vs) = ConvexPolygon <$> traverse1 f vs

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

instance HasHoles (ConvexPolygonF f point)

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Point_ point 2 r
         ) => Polygon_ (ConvexPolygonF f point) point r where
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


instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfSpaceF line) (ConvexPolygonF f point) where
  halfPlane `intersects` poly = halfPlane `intersects` (toSimplePolygon poly)
    -- TODO there is a better, O(log n) time implementation. use that instead ...

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
maxInDirection u = findMaxWith (cmpInDirection2 u)

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

--------------------------------------------------------------------------------

instance ( HasInPolygon (ConvexPolygonF f point) point r, Num r, Ord r)
         => Point 2 r `HasIntersectionWith` ConvexPolygonF f point where
  q `intersects` poly = q `inPolygon` poly /= StrictlyOutside

type instance Intersection (Point 2 r) (ConvexPolygonF f point) = Maybe (Point 2 r)

instance ( HasInPolygon (ConvexPolygonF f point) point r, Num r, Ord r
         ) => Point 2 r `IsIntersectableWith` ConvexPolygonF f point where
  q `intersect` poly | q `intersects` poly = Just q
                     | otherwise           = Nothing

--------------------------------------------------------------------------------

instance ( Num r, Ord r
         , ConvexPolygon_ (ConvexPolygonF nonEmpty vertex) vertex r
         )
         => LinePV 2 r `HasIntersectionWith` ConvexPolygonF nonEmpty vertex where
  l `intersects` poly = case (onSide p l, onSide q l) of
                          (OnLine, _) -> True
                          (_, OnLine) -> True
                          (sp, sq)    -> sp /= sq
    where
      (p,q) = extremes (perpendicularTo l ^. direction) poly


type instance Intersection (LinePV 2 r) (ConvexPolygonF nonEmpty vertex) =
  Maybe (PossiblyDegenerateSegment (Point 2 r) (ClosedLineSegment (Point 2 r)))

type instance Intersection (LinePV 2 r)
                           (PossiblyDegenerateSimplePolygon vertex
                              (ConvexPolygonF nonEmpty vertex)) =
  Maybe (PossiblyDegenerateSegment (Point 2 r) (ClosedLineSegment (Point 2 r)))

instance ( Fractional r, Ord r
         , ConvexPolygon_ (ConvexPolygonF nonEmpty vertex) vertex r
         )
         => LinePV 2 r `IsIntersectableWith` ConvexPolygonF nonEmpty vertex where
  l `intersect` poly = case mapMaybe (l `intersect`) edgeSegs of
      []      -> Nothing
      [ex]    -> Just $ case ex of
        Line_x_LineSegment_Point p -> SinglePoint p
        Line_x_LineSegment_LineSegment e -> ActualSegment (asClosed e)
      [e1,e2] -> Just $ case e1 of
        Line_x_LineSegment_LineSegment e -> ActualSegment (asClosed e)
        Line_x_LineSegment_Point p       -> case e2 of
          Line_x_LineSegment_Point q       -> ActualSegment (ClosedLineSegment p q)
          Line_x_LineSegment_LineSegment e -> ActualSegment (asClosed e)
      _       -> error "line x convexPolygon intersection. absurd"
    where
      edgeSegs = asHalfOpen <$> poly^..outerBoundaryEdgeSegments
      asHalfOpen s = LineSegment (AnOpenE (s^.start.asPoint)) (AnClosedE (s^.end.asPoint))
      asClosed s   = ClosedLineSegment (s^.start) (s^.end)


instance ( Fractional r, Ord r
         , ConvexPolygon_ (ConvexPolygonF nonEmpty vertex) vertex r
         )
          => LinePV 2 r `IsIntersectableWith`
               PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF nonEmpty vertex) where
  intersect l = \case
    DegenerateVertex v
     | v `onLine` l    -> Just $ SinglePoint (v^.asPoint)
     | otherwise       -> Nothing
    DegenerateEdge e   -> fmap wrap $ l `intersect` (view asPoint <$> e)
    ActualPolygon poly -> l `intersect` poly

wrap :: LineLineSegmentIntersection seg
     -> PossiblyDegenerateSegment (Point 2 (NumType seg)) seg
wrap = \case
  Line_x_LineSegment_Point p         -> SinglePoint p
  Line_x_LineSegment_LineSegment seg -> ActualSegment seg
-- TODO: Unify these types

--------------------------------------------------------------------------------

-- | A HalfPlane and a Convex polygon intersect in a single component, which is a
-- possiblyDegenerate convex polygon.
type instance Intersection (HalfSpaceF line) (ConvexPolygonF f point) =
  Maybe (HalfPlaneConvexPolygonIntersection f (NumType point) point)

-- | A single Component of a HalfPlane x ConvexPolygon intersection.
type HalfPlaneConvexPolygonIntersection f r vertex =
  PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f (OriginalOrExtra vertex (Point 2 r)))

-- | If we drag along extra information in the halfplane polygon intersection we lose it
type instance Intersection (HalfSpaceF line :+ extra) (ConvexPolygonF f point :+ extra') =
  Intersection (HalfSpaceF line) (ConvexPolygonF f point)


--------------------------------------------------------------------------------
-- * Intersection between Points  and possibly degenerate convex polygons

instance ( Point_ vertex 2 r, Num r, Ord r, VertexContainer f vertex
         , HyperPlane_ line 2 r
         ) => HalfSpaceF line `HasIntersectionWith`
              PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex) where
  halfSpace `intersects` degenPoly = case degenPoly of
    DegenerateVertex v -> (v^.asPoint) `intersects` halfSpace
    DegenerateEdge e   -> e `intersects` halfSpace
    ActualPolygon poly -> halfSpace `intersects` poly

--------------------------------------------------------------------------------

instance ( IsIntersectableWith (HalfSpaceF line) (ConvexPolygonF f vertex)
         , HasIntersectionWith (HalfSpaceF line :+ extra) (ConvexPolygonF f vertex :+ extra')
         ) => IsIntersectableWith (HalfSpaceF line :+ extra)
                                  (ConvexPolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersect` (poly :+ _) = halfPlane `intersect` poly

instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (ConvexPolygonF f vertex) where
  halfPlane `intersect` poly = case comps of
      []  -> Nothing
      [c] -> Just c
      _   -> error "halfplane x convexPolygon intersection: absurd."
    where
      comps = collectComponents (halfPlane^.boundingHyperPlane)
            . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
            $ toNonEmptyOf vertices poly
  -- halfPlane `intersect` poly = case halfPlane `intersect` (toSimplePolygon poly) of
  --   [comp] -> Just $ ConvexPolygon <$> comp
  --             -- note that the intersection between a halfspace and a convex polygon
  --             -- is indeed guaranteed to be convex. Hence the 'ConvexPolygon' call here
  --             -- is safe.
  --   _      -> Nothing


-- | Collect the connected components
collectComponents   :: forall cyclic f vertex r. ( Point_ vertex 2 r, Ord r, Fractional r
                       , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                       , Traversable1 cyclic, HasFromFoldable1 f
                       )
                    => LinePV 2 r -- ^ the bounding line of the halfplane
                    -> cyclic (Bool, NonEmpty vertex)
                    -> [HalfPlaneConvexPolygonIntersection f r vertex]
collectComponents l = foldMapOf (asFold1 withCyclicNeighbours) f
  where
    -- We go through the components with their neighbours. Each component
    -- is a non-empty list of vertices in CCW order along the polygon.
    --
    -- For each component [v1,..,vn] we may need to add two vertices; the intersection
    -- point of l with the edge between the last vertex um of the previous component and
    -- v1, and the intersection point of l with the edge between vn and the first vertex
    -- w1 of the next component
    f :: ((Bool, NonEmpty vertex), V2 (Bool, NonEmpty vertex))
      -> [HalfPlaneConvexPolygonIntersection f r vertex]
    f ((b, current@(v1 :| rest)), V2 (_, NonEmpty.last -> um) (_, w1 :| _))
      | not b     = []
      | otherwise = let vn     = NonEmpty.last current
                        extras = mapMaybe (intersectionPoint l) [(vn,w1), (um,v1)]
                    in pure $ case (NonEmpty.nonEmpty extras,NonEmpty.nonEmpty rest) of
                       (Nothing, Nothing)        -> DegenerateVertex v1
                       (Nothing, Just (p :| [])) -> DegenerateEdge
                                                  $ ClosedLineSegment p v1
                       (extras',  _)             -> ActualPolygon poly
                         where
                           poly = uncheckedFromCCWPoints
                                $ (fmap Extra <$> extras') <<> (Original <$> current)

-- | Helper to combine at most two a's into one
(<<>)     :: Semigroup a => Maybe a -> a -> a
xs <<> ys = case xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys

-- | Compute the intersection between a line and the "edge" given by the two vertices.
-- We treat the edge as open; i.e. we only report the intersection if it is interior
intersectionPoint            :: (Point_ vertex 2 r, Ord r, Fractional r)
                             => LinePV 2 r -> (vertex, vertex) -> Maybe (Point 2 r)
intersectionPoint line (u,v) = case line `intersect` OpenLineSegment u v of
                                 Just (Line_x_LineSegment_Point p) -> Just p
                                 _                                 -> Nothing

-- | Convert a traversal into a fold.
asFold1   :: Traversal1 s t a b -> Fold1 s a
asFold1 t = \aFa -> phantom . t (phantom . aFa)

--------------------------------------------------------------------------------
-- * Intersecting a Halfspace and a Possibly Degenerate Convex Polygon

-- | Intersecting a halfplane witha possibly degenerate convex polygon
-- gives us a possibly degenerate polygon again.
--
type instance Intersection (HalfSpaceF line)
                           (PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex))
  = Maybe (PossiblyDegenerateSimplePolygon
              (OriginalOrExtra vertex (CanonicalPoint vertex))
              (ConvexPolygonF f (OriginalOrExtra vertex (CanonicalPoint vertex)))
          )
   -- we lose some information here; if we are a degenreate point we are guaranteed
   -- to be an original; the type also allows it to be an Extra.

instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HyperPlane_ line 2 r
         , IsIntersectableWith (HalfSpaceF line) (ConvexPolygonF f vertex)
         -- this one is satisfied for e.g. line ~ LinePV

         , IsIntersectableWith (LinePV 2 r) line
         , Intersection (LinePV 2 r) line ~ Maybe (LineLineIntersectionG r line')
         ) => HalfSpaceF line `IsIntersectableWith`
              PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex) where
  halfSpace `intersect` degenPoly = case degenPoly of
    DegenerateVertex v -> DegenerateVertex (Original v) <$ ((v^.asPoint) `intersect` halfSpace)
    DegenerateEdge e   -> e `intersect` halfSpace <&> \case
      ClosedLineSegment_x_HalfSpace_Point v           -> DegenerateVertex (Original v)
      ClosedLineSegment_x_HalfSpace_SubSegment s      -> DegenerateEdge s
      ClosedLineSegment_x_HalfSpace_CompleteSegment _ -> DegenerateEdge (Original <$> e)

    ActualPolygon poly -> first Original <$> halfSpace `intersect` poly

--------------------------------------------------------------------------------
-- * Intersection of Triangle and ConvexPolygon

type instance Intersection (Triangle corner) (ConvexPolygonF f vertex) =
  Maybe (PossiblyDegenerateSimplePolygon (OriginalOrCanonical vertex)
                                         (ConvexPolygonF f (OriginalOrCanonical vertex))
        )

instance ( Point_ point 2 r, Point_ point' 2 r, Num r, Ord r, VertexContainer f point
         , VertexContainer f (Point 2 r)
         ) => HasIntersectionWith (Triangle point') (ConvexPolygonF f point) where
  triangle `intersects` poly = anyOf vertices                  (`intersects` tri) poly'
                              || anyOf outerBoundaryEdgeSegments (`intersects` tri) poly'
    where
      poly' :: ConvexPolygonF f (Point 2 r)
      poly' = poly&vertices %~ view asPoint
      tri = (^.asPoint) <$> triangle


type V vertex r = OriginalOrExtra vertex (Point 2 r)

instance ( Point_ vertex 2 r, Point_ corner 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (Point 2 r)
         , VertexContainer f (OriginalOrCanonical vertex)
         , VertexContainer f (OriginalOrExtra (OriginalOrCanonical vertex) (Point 2 r))
         , HasFromFoldable1 f
         ) => IsIntersectableWith (Triangle corner) (ConvexPolygonF f vertex) where
  triangle `intersect` poly = foldr intersect' (Just $ ActualPolygon $ poly&vertices %~ Original)
                                               (Triangle.intersectingHalfPlanes triangle)

-- | Helper to repeatedly intersect a halfplane and a convex polygon
intersect'      :: ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
                   , VertexContainer f (Point 2 r)
                   , VertexContainer f (OriginalOrCanonical vertex)
                   , VertexContainer f (OriginalOrExtra (OriginalOrCanonical vertex) (Point 2 r))
                   , HasFromFoldable1 f
                   )
                => HalfSpaceF (LinePV 2 r)
                -> Maybe (PossiblyDegenerateSimplePolygon (V vertex r)
                                                          (ConvexPolygonF f (V vertex r)))
                -> Maybe (PossiblyDegenerateSimplePolygon (V vertex r)
                                                          (ConvexPolygonF f (V vertex r)))
intersect' h mp = do p <- mp
                     bimap flatten (fmap flatten) <$> h `intersect` p

-- | Flatten two nested originals
flatten :: OriginalOrExtra (OriginalOrExtra vertex extra) extra -> OriginalOrExtra vertex extra
flatten = \case
  Extra e    -> Extra e
  Original o -> o


-- * Intersection of Rectangle and ConvexPolygon

type instance Intersection (Rectangle corner) (ConvexPolygonF f vertex) =
  Maybe (PossiblyDegenerateSimplePolygon (OriginalOrCanonical vertex)
                                         (ConvexPolygonF f (OriginalOrCanonical vertex))
        )

instance ( Point_ point 2 r, Point_ point' 2 r, Num r, Ord r, VertexContainer f point
         , VertexContainer f (Point 2 r)
         ) => HasIntersectionWith (Rectangle point') (ConvexPolygonF f point) where
  rectangle `intersects` poly =
       anyOf vertices                  (`intersects` rect) poly'
    || anyOf outerBoundaryEdgeSegments (`intersects` rect) poly'
    where
      poly' :: ConvexPolygonF f (Point 2 r)
      poly' = poly&vertices %~ view asPoint
      rect  = (^.asPoint) <$> rectangle


instance ( Point_ vertex 2 r, Point_ corner 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (Point 2 r)
         , VertexContainer f (OriginalOrCanonical vertex)
         , VertexContainer f (OriginalOrExtra (OriginalOrCanonical vertex) (Point 2 r))
         , HasFromFoldable1 f
         ) => IsIntersectableWith (Rectangle corner) (ConvexPolygonF f vertex) where
  rect `intersect` poly = foldr intersect' (Just $ ActualPolygon $ poly&vertices %~ Original)
                                           (Box.intersectingHalfPlanes rect)

--------------------------------------------------------------------------------
-- * Halfspace x Rectangle Intersection

type instance Intersection (HalfSpaceF line) (Rectangle corner) =
  Maybe (PossiblyDegenerateSimplePolygon (CanonicalPoint corner)
                                         (ConvexPolygon (CanonicalPoint corner))
        )

-- this type is not entirely right; as we need to constrain the dimension to 2

instance ( Point_ corner 2 r, Num r, Ord r
         ) => HasIntersectionWith (HalfSpaceF (LinePV 2 r)) (Rectangle corner) where
  halfPlane `intersects` rect' = any (`intersects` halfPlane) ((^.asPoint) <$> Box.corners rect')

instance ( Point_ corner 2 r, Fractional r, Ord r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (Rectangle corner) where
  halfPlane `intersect` rect' = fmap (fmap flatten')
                             <$> halfPlane `intersect` (toConvexPolygon rect')
    where
      flatten' = \case
        Original p -> p
        Extra p    -> p

      toConvexPolygon :: Rectangle corner -> ConvexPolygon (Point 2 r)
      toConvexPolygon = uncheckedFromCCWPoints . fmap (^.asPoint) . Box.corners

--------------------------------------------------------------------------------
-- * Halfspace x Triangle Intersection

type instance Intersection (HalfSpaceF line) (Triangle corner) =
  Maybe (PossiblyDegenerateSimplePolygon (CanonicalPoint corner)
                                         (ConvexPolygon (CanonicalPoint corner))
        )
-- this type is not entirely right; as we need to constrain the dimension to 2

instance ( Point_ corner 2 r, Num r, Ord r
         ) => HasIntersectionWith (HalfSpaceF (LinePV 2 r)) (Triangle corner) where
  halfPlane `intersects` tri = anyOf (Triangle.corners.traverse1.asPoint)
                                     (`intersects` halfPlane) tri

instance ( Point_ corner 2 r, Fractional r, Ord r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (Triangle corner) where
  halfPlane `intersect` tri = fmap (fmap flatten')
                           <$> halfPlane `intersect` (toConvexPolygon tri)
    where
      flatten' = \case
        Original p -> p
        Extra p    -> p

      toConvexPolygon :: Triangle corner -> ConvexPolygon (Point 2 r)
      toConvexPolygon = uncheckedFromCCWPoints . fmap (^.asPoint) . view (Triangle.corners)
                      . toCounterClockwiseTriangle
