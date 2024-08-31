{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple
  ( SimplePolygon_(..)
  , SimplePolygon
  , SimplePolygonF(..)
  , toCyclic
  , VertexContainer
  , HasInPolygon(..)
  , inSimplePolygon
  , hasNoSelfIntersections
  , module HGeometry.Polygon.Simple.Class
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           HGeometry.Boundary
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import           HGeometry.Intersection
import           HGeometry.LineSegment.Intersection.BentleyOttmann
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.Type
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

instance ( VertexContainer f point
         ) => HasVertices (SimplePolygonF f point) (SimplePolygonF f point') where
  vertices = _SimplePolygonF . traversed1

instance ( VertexContainer f point
         ) => HasVertices' (SimplePolygonF f point) where
  type Vertex   (SimplePolygonF f point) = point
  type VertexIx (SimplePolygonF f point) = Int
  vertexAt i = _SimplePolygonF . iix i
  numVertices = F.length . view _SimplePolygonF

-- instance VertexContainer f point => HasEdges' (SimplePolygonF f point) where
--   -- ^ An edge (v_i,v_{i+1})
--   type Edge   (SimplePolygonF f point) = (point, point)
--    -- ^ every edge (v_i,v_{i+1}) is identified by (the index of) its preceding vertex v_i
--   type EdgeIx (SimplePolygonF f point) = Int
--   edgeAt i = \pEfE pg -> indexed pEfE i (pg^?!vertexAt i, pg^?!vertexAt (succ i))
--                          <&> \(u',v') -> pg&vertexAt i        .~ u'
--                                            &vertexAt (succ i) .~ v'
--     -- we first just run the function pEfE on the particular edge in question,
--     -- we get some updated vertices out of this; which we then update appropriately.
--   {-# INLINE edgeAt #-}
--   numEdges = numVertices

-- instance VertexContainer f point
--        => HasEdges (SimplePolygonF f point) (SimplePolygonF f point) where
--   -- ^ Warning for when using this as a traversal: when applying the function on an edge
--   -- (v_i,v_{i+1}) that modifies vertex v_{i+1}, this modification is discarded.
--   edges = \pEfE pg -> let pvFv i v = fst <$> indexed pEfE i (v, pg^?!vertexAt (succ i))
--                       in vertices pvFv pg
--
-- I've uncommented these for now; since the above behaviour would be inconsistent between
-- edgeAt (which would modify both vertices), and edges, which would modify only one vertex.
-- that is a bit too weird.

instance ( VertexContainer f point
         ) => HasOuterBoundary (SimplePolygonF f point) where
  outerBoundary = _SimplePolygonF . traversed1
  outerBoundaryVertexAt i = singular (vertexAt i)

  ccwOuterBoundaryFrom i = _SimplePolygonF.traverseRightFrom i
  cwOuterBoundaryFrom  i = _SimplePolygonF.traverseLeftFrom  i

instance HasHoles (SimplePolygonF f point)

instance ( Point_ point 2 r
         , HasFromFoldable1 f
         , VertexContainer f point
         ) => Polygon_ (SimplePolygonF f point) point r where

  ccwPredecessorOf u = \pvFv pg -> let n = numVertices pg
                                       p = (pred u) `mod` n
                                       l = singular $ vertexAt p
                                   in l pvFv pg
  -- make sure to wrap the index to make sure we report the right index.
  ccwSuccessorOf   u = \pvFv pg -> let n = numVertices pg
                                       s = (succ u) `mod` n
                                       l = singular $ vertexAt s
                                   in l pvFv pg

instance ( Point_ point 2 r
         , VertexContainer f point
         , HasFromFoldable1 f
         ) => SimplePolygon_ (SimplePolygonF f point) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable1
                         . NonEmpty.fromList . F.toList

  fromPoints pts = case F.toList pts of
    pts'@(_ : _ : _ : _ ) -> Just . toCounterClockwiseOrder . uncheckedFromCCWPoints $ pts'
                             -- TODO: verify that:
                              --      we have no repeated vertices,
                             --      no self intersections, and
                             --      not all vertices are colinear
    _                     -> Nothing -- we need at least three vertices

instance ( Show point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Show (SimplePolygonF f point) where
  showsPrec = showsPrecSimplePolygon "SimplePolygon"

instance ( Read point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Read (SimplePolygonF f point) where
  readsPrec = readsPrecSimplePolygon "SimplePolygon"


{-
instance (SimplePolygon_ (SimplePolygonF f) point r, Fractional r, Ord r)
         => HasSquaredEuclideanDistance (SimplePolygonF f point) where
  pointClosestToWithDistance = pointClosestToWithDistanceSimplePolygon
-}

--------------------------------------------------------------------------------

_testPoly :: SimplePolygon (Point 2 Int)
_testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------

instance SimplePolygon_ (SimplePolygonF f point) point r
         => HasInPolygon (SimplePolygonF f point) point r

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => HasIntersectionWith (Point 2 r) (SimplePolygonF f point) where
  q `intersects` pg = q `inSimplePolygon` pg /= StrictlyOutside

type instance Intersection (Point 2 r) (SimplePolygonF f point) = Maybe (Point 2 r)

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => IsIntersectableWith (Point 2 r) (SimplePolygonF f point) where
  q `intersect` pg | q `intersects` pg = Just q
                   | otherwise         = Nothing
  -- this implementation is a bit silly but ok


--------------------------------------------------------------------------------

-- | verify that some sequence of points has no self intersecting edges.
hasNoSelfIntersections    :: forall f point r.
                             (Foldable f, Functor f, Point_ point 2 r, Ord r, Real r)
                          => f point -> Bool
hasNoSelfIntersections vs = let vs' = (\p -> (p^.asPoint)&coordinates %~ toRational) <$> vs
                                pg :: SimplePolygon (Point 2 Rational)
                                pg = uncheckedFromCCWPoints vs'
                            in Map.null $ interiorIntersections $ pg^..outerBoundaryEdgeSegments
  -- outerBoundaryEdgeSegments interiorIntersections pg
