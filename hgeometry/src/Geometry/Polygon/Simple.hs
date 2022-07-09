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
module Geometry.Polygon.Simple
  ( SimplePolygon_(..)
  , SimplePolygon
  , SimplePolygonF
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           GHC.Generics
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class
import           Geometry.Properties

import           Data.Maybe
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Circular.Util as CV

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

-- | By default we store simple polygons as circular vectors.
type SimplePolygon = SimplePolygonF CircularVector

type instance Dimension (SimplePolygonF f point r) = 2
type instance NumType   (SimplePolygonF f point r) = r

deriving instance Eq (f (point 2 r)) => Eq (SimplePolygonF f point r)

-- instance Wrapped   (SimplePolygonF f point r)
-- instance Rewrapped (SimplePolygonF f point r) (f (point 2 r))

-- | Access the container
_SimplePolygonF :: Iso (SimplePolygonF f point r) (SimplePolygonF f' point' r')
                       (f (point 2 r))              (f' (point' 2 r'))
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon

-- instance TraversableWithIndex Int f
--       => HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r') where
--   type Vertex   (SimplePolygonF f point r) = point 2 r
--   type VertexIx (SimplePolygonF f point r) = Int
--   vertices = _SimplePolygonF . itraversed

instance HasVertices (SimplePolygon point r) (SimplePolygon point' r') where
  type Vertex   (SimplePolygon point r) = point 2 r
  type VertexIx (SimplePolygon point r) = Int
  vertices = _SimplePolygonF . CV.itraversedRight

instance ( Point_ point 2 r
         ) => SimplePolygon_ (SimplePolygonF CircularVector) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromMaybe err . CV.fromList . F.toList
    where
      err = error "uncheckedFromCCWPoints, circular vector: no vertices found!"


testPoly :: SimplePolygonF [] Point Int
testPoly = MkSimplePolygon [Point2 10 20, origin]



--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point r =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point r }

type ConvexPolygon = ConvexPolygonF CircularVector

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

-- | \( O(n) \) Verify that a convex polygon is strictly convex.
verifyConvex :: (Ord r, Num r) => ConvexPolygonF f point r -> Bool
verifyConvex = isStrictlyConvex . toSimplePolygon
