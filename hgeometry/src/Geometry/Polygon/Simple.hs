{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
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
import           Data.Coerce
import           Data.Cyclic
import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup.Foldable
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class
import           Geometry.Polygon.Simple.Implementation
import           Geometry.Properties
import           Geometry.Transformation

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

-- | By default we store simple polygons as non-empty circular vectors.
type SimplePolygon = SimplePolygonF (Cyclic NonEmptyVector)

type instance Dimension (SimplePolygonF f point r) = 2
type instance NumType   (SimplePolygonF f point r) = r

deriving instance Eq (f (point 2 r)) => Eq (SimplePolygonF f point r)


-- instance Wrapped   (SimplePolygonF f point r)
-- instance Rewrapped (SimplePolygonF f point r) (f (point 2 r))

-- | Access the container
_SimplePolygonF :: Iso (SimplePolygonF f point r) (SimplePolygonF f' point' r')
                       (f (point 2 r))              (f' (point' 2 r'))
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon

instance TraversableWithIndex Int f
      => HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r') where
  vertices = _SimplePolygonF . itraversed

instance TraversableWithIndex Int f =>
         HasPoints (SimplePolygonF f point r) (SimplePolygonF f point' r') point point' where
  allPoints = vertices

-- instance ( TraversableWithIndex Int f
--          , HasPoints (SimplePolygonF f point r) (SimplePolygonF f point r)
--          ) => IsTransformable (SimplePolygonF f point r)

-- instance HasVertices (SimplePolygon point r) (SimplePolygon point' r') where
--   type Vertex   (SimplePolygon point r) = point 2 r
--   type VertexIx (SimplePolygon point r) = Int
--   -- vertices = _SimplePolygonF . CV.itraversedRight




instance ( TraversableWithIndex Int f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         )
      => HasVertices' (SimplePolygonF f point r) where
  type Vertex   (SimplePolygonF f point r) = point 2 r
  type VertexIx (SimplePolygonF f point r) = Int
  vertexAt i = _SimplePolygonF . iix i


instance ( TraversableWithIndex Int f
         , Traversable1 f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         )
      => HasOuterBoundary (SimplePolygonF f point r) where
  outerBoundary = _SimplePolygonF . traversed1
  outerBoundaryVertexAt i = singular (vertexAt i)

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , Traversable1 f
         , HasFromFoldable1 f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         ) => Polygon_ (SimplePolygonF f) point r where
  area = areaSimplePolygon

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , Traversable1 f
         , HasFromFoldable1 f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         ) => SimplePolygon_ (SimplePolygonF f) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable1
                         . NonEmpty.fromList . F.toList


instance ( Show (point 2 r)
         , SimplePolygon_ (SimplePolygonF f) point r
         ) => Show (SimplePolygonF f point r) where
  show = showSimplePolygon


instance HasSquaredEuclideanDistance (SimplePolygonF f point r) where
  pointClosestToWithDistance q = pointClosestToWithDistance q . toSimplePolygon


-- instance HasAdjacencies (SimplePolygonF f point r) where


--------------------------------------------------------------------------------


testPoly :: SimplePolygon Point Int
testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------
