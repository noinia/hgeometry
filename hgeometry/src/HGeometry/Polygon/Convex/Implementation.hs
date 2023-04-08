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
  ) where

import Control.DeepSeq (NFData)
import Control.Lens
import HGeometry.Cyclic
import Data.Vector.NonEmpty (NonEmptyVector)
import HGeometry.Vector.NonEmpty.Util ()
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Simple.Implementation
import HGeometry.Properties
import Hiraffe.Graph

--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point }
  deriving newtype (NFData, Eq)

-- | By default we use a cyclic non-empty vector to represent convex polygons.
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

instance ( HasVertices (SimplePolygonF f point) (SimplePolygonF f point')
         ) => HasVertices (ConvexPolygonF f point) (ConvexPolygonF f point') where
  vertices = _UncheckedConvexPolygon . vertices

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

-- | Smart constructor to construct a striclty convex polygon from a
-- simple polygon.
fromSimplePolygon :: (Ord r, Num r, Point_ point 2 r, VertexContainer f point)
                  => SimplePolygonF f point -> Maybe (ConvexPolygonF f point)
fromSimplePolygon pg
  | isStrictlyConvex pg = Just (ConvexPolygon pg)
  | otherwise           = Nothing


instance ( Show point
         , SimplePolygon_ (ConvexPolygonF f point) point r
         ) => Show (ConvexPolygonF f point) where
  show = showSimplePolygon






{-
instance ( SimplePolygon_ (ConvexPolygonF f point) point r
         , SimplePolygon_ (SimplePolygonF f point) point r
         , Ord r, Fractional r)
       => HasSquaredEuclideanDistance (ConvexPolygonF f point) where
  pointClosestToWithDistance q = pointClosestToWithDistance q . toSimplePolygon
  -- FIXME: we should be able to implement this in O(log n) time instead!!
-}
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
