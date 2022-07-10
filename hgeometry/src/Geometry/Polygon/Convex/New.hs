{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Simple
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Convex.New
  (

  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           GHC.Generics
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple
import           Geometry.Polygon.Simple.Class
import           Geometry.Polygon.Simple.Implementation
import           Geometry.Properties

import           Data.Maybe
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV


--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point r =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point r }

type ConvexPolygon = ConvexPolygonF (Cyclic Vector.Vector)

-- | ConvexPolygons are isomorphic to SimplePolygons with the added
--   constraint that they have no reflex vertices.
--
-- Note that this is unchecked; i.e. one can turn an arbitrary simple polygon
-- into a suposedly convex one.
_UncheckedConvexPolygon :: Iso (ConvexPolygonF f point r) (ConvexPolygonF f' point' s)
                               (SimplePolygonF f point r) (SimplePolygonF f' point' s)
_UncheckedConvexPolygon = iso toSimplePolygon ConvexPolygon

-- | Prism that can forget that the polygon is convex
--
_ConvexPolygon :: forall f point r. (Num r, Ord r)
               => Prism' (SimplePolygonF f point r) (ConvexPolygonF f point r)
_ConvexPolygon = prism' toSimplePolygon fromPolygon
  where
    fromPolygon                          :: SimplePolygonF f point r
                                         -> Maybe (ConvexPolygonF f point r)
    fromPolygon pg | isStrictlyConvex pg = Just (ConvexPolygon pg)
                   | otherwise           = Nothing

-- deriving instance Eq (ConvexPolygonF f point r)
-- | Polygons are per definition 2 dimensional
type instance Dimension (ConvexPolygonF f point r) = 2
type instance NumType   (ConvexPolygonF f point r) = r

instance ( HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r')
         )
      => HasVertices (ConvexPolygonF f point r) (ConvexPolygonF f point' r') where
  type Vertex   (ConvexPolygonF f point r) = Vertex   (SimplePolygonF f point r)
  type VertexIx (ConvexPolygonF f point r) = VertexIx (SimplePolygonF f point r)
  vertices = _UncheckedConvexPolygon . vertices

instance HasVertices' (SimplePolygonF f point r) => HasVertices' (ConvexPolygonF f point r) where
  vertexAt i = _UncheckedConvexPolygon . vertexAt i

instance ( HasOuterBoundary (SimplePolygonF f point r)
         , VertexIx (SimplePolygonF f point r) ~ Int
         ) =>
         HasOuterBoundary (ConvexPolygonF f point r) where
  outerBoundary = _UncheckedConvexPolygon . outerBoundary
  outerBoundaryVertexAt i = _UncheckedConvexPolygon . outerBoundaryVertexAt i



instance ( SimplePolygon_ (SimplePolygonF f) point r
         , Point_ point 2 r
         ) => SimplePolygon_ (ConvexPolygonF f) point r where
  -- | Additional precondition: the points actually form a convex polygon
  uncheckedFromCCWPoints = ConvexPolygon . uncheckedFromCCWPoints
  fromPoints = fmap ConvexPolygon . fromPoints
    -- FIXME: here we could actually test if the thing is convex

-- -- | Smart constructor to construct a convex polygon from a simple polygon.
-- fromSimplePolygon :: SimplePolygonF f point r -> Maybe (ConvexPolygonF f point r)
-- fromSimplePolygon pg
--   | isConvex pg = Just (ConvexPolygon pg)
--   | otherwise   = Nothing


--------------------------------------------------------------------------------

-- | Verify that a convex polygon is strictly convex.
--
-- running time \( O(n) \)
verifyConvex :: (Ord r, Num r) => ConvexPolygonF f point r -> Bool
verifyConvex = isStrictlyConvex . toSimplePolygon


-- | \( O(n) \) Check if a polygon is strictly convex.
isStrictlyConvex   :: (Ord r, Num r) => SimplePolygonF f point r -> Bool
isStrictlyConvex s = undefined
  --   CV.and (CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs))
  -- where
  --   f a b c = ccw' a b c == CCW
  --   vs = s ^. outerBoundaryVector

-- | \( O(n) \) Check if a polygon is convex.
isConvex :: (Ord r, Num r) => SimplePolygonF f point r -> Bool
isConvex s = undefined
