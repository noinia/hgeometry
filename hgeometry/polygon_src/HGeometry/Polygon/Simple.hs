{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
  , SimplePolygonF
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           HGeometry.Cyclic
import qualified Data.Foldable as F
import           HGeometry.Foldable.Util
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           HGeometry.Vector.NonEmpty.Util ()
import           GHC.Generics
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Properties
import           Hiraffe.Graph
--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point = MkSimplePolygon (f point)
  deriving (Generic)
  deriving newtype (NFData)

-- | By default we store simple polygons as non-empty circular vectors.
type SimplePolygon = SimplePolygonF (Cyclic NonEmptyVector)

type instance Dimension (SimplePolygonF f point) = 2
type instance NumType   (SimplePolygonF f point) = NumType point

deriving instance Eq (f point) => Eq (SimplePolygonF f point)

-- | Access the container
_SimplePolygonF :: Iso (SimplePolygonF f point) (SimplePolygonF f' point')
                       (f point)                (f' point' )
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon

instance (TraversableWithIndex Int f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         )
      => HasVertices (SimplePolygonF f point) (SimplePolygonF f point') where
  vertices = _SimplePolygonF . itraversed

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         ) => HasPoints (SimplePolygonF f point) (SimplePolygonF f point') point point' where
  allPoints = _SimplePolygonF . traversed1

-- instance ( TraversableWithIndex Int f
--          , HasPoints (SimplePolygonF f point r) (SimplePolygonF f point r)
--          ) => IsTransformable (SimplePolygonF f point r)

-- instance HasVertices (SimplePolygon point r) (SimplePolygon point' r') where
--   type Vertex   (SimplePolygon point r) = point 2 r
--   type VertexIx (SimplePolygon point r) = Int
--   -- vertices = _SimplePolygonF . CV.itraversedRight




instance ( TraversableWithIndex Int f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         )
      => HasVertices' (SimplePolygonF f point) where
  type Vertex   (SimplePolygonF f point) = point
  type VertexIx (SimplePolygonF f point) = Int
  vertexAt i = _SimplePolygonF . iix i


instance ( TraversableWithIndex Int f
         , Traversable1 f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         )
      => HasOuterBoundary (SimplePolygonF f point) where
  outerBoundary = _SimplePolygonF . traversed1
  outerBoundaryVertexAt i = singular (vertexAt i)

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , Traversable1 f
         , HasFromFoldable1 f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         ) => Polygon_ (SimplePolygonF f point) point r where
  area = areaSimplePolygon

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , Traversable1 f
         , HasFromFoldable1 f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         ) => SimplePolygon_ (SimplePolygonF f point) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable1
                         . NonEmpty.fromList . F.toList


instance ( Show point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Show (SimplePolygonF f point) where
  show = showSimplePolygon

{-
instance (SimplePolygon_ (SimplePolygonF f) point r, Fractional r, Ord r)
         => HasSquaredEuclideanDistance (SimplePolygonF f point) where
  pointClosestToWithDistance = pointClosestToWithDistanceSimplePolygon
-}
--------------------------------------------------------------------------------

_testPoly :: SimplePolygon (Point 2 Int)
_testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------





class AsCyclic v where
  {-# MINIMAL itraverseRightFrom, itraverseLeftFrom #-}
  -- | Given an index i, returns an "rightwards" (i.e. clockwise)
  -- IndexedTraversal of the cyclic structure, starting from the
  -- element with index i.
  itraverseRightFrom :: Int -> IndexedTraversal Int (Cyclic v a) (Cyclic v b) a b

  -- | Given an index i, returns an "leftwards" (i.e. counter
  -- clockwise) IndexedTraversal of the cyclic structure, starting
  -- from the element with index i.
  itraverseLeftFrom   :: Int -> IndexedTraversal Int (Cyclic v a) (Cyclic v b) a b
  -- itraverseLeftFrom i =


  -- -- | Given an index i, returns an "rightwards" (i.e. clockwise)
  -- -- IndexedFold of the cyclic structure, starting from the
  -- -- element with index i.
  -- ifoldRightFrom :: Int -> IndexedFold Int (Cyclic v a) a
  -- ifoldRightFrom = itraverseRightFrom

  -- -- | Given an index i, returns an "leftward" (i.e. count clockwise)
  -- -- IndexedFold of the cyclic structure, starting from the
  -- -- element with index i.
  -- ifoldLeftFrom :: Int -> IndexedFold Int (Cyclic v a) a
  -- ifoldLeftFrom = itraverseLeftFrom

-- instance AsCyclic NonEmptyVector where
--   itraverseRightFrom i pafb (Cyclic v) = Cyclic <$> do v' <-

--     [0..n]
--     where
--       n = length cv
