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


  , Cyclic(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Functor.Apply as Apply
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup.Foldable
import           GHC.Generics
import           Geometry.Point
import           Geometry.Transformation
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class
import           Geometry.Polygon.Simple.Implementation
import           Geometry.Properties

import           Data.Coerce
import           Data.Maybe
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))

--------------------------------------------------------------------------------

class HasFromFoldable f where
  fromFoldable :: Foldable g => g a -> f a
  fromFoldable = fromList . F.toList

  fromList :: [a] -> f a
  {-# MINIAL fromList #-}

-- instance HasFromFoldable1 [] where
--   fromList = id

class HasFromFoldable1 f where
  fromFoldable1 :: Foldable1 g => g a -> f a
  fromFoldable1 = fromNonEmpty . toNonEmpty

  fromNonEmpty :: NonEmpty a -> f a
  {-# MINIAL fromNonEmpty #-}

instance HasFromFoldable1 NonEmpty where
  fromNonEmpty = id

instance HasFromFoldable Vector.Vector  where
  fromList = Vector.fromList

instance HasFromFoldable1 NonEmptyVector  where
  fromNonEmpty = NonEmptyVector.fromNonEmpty


--------------------------------------------------------------------------------


type instance Index   (NonEmptyVector a) = Int
type instance IxValue (NonEmptyVector a) = a

instance Ixed (NonEmptyVector a) where
  ix i f (NonEmptyVector v) = NonEmptyVector <$> ix i f v

instance Foldable1 NonEmptyVector
instance Traversable1 NonEmptyVector where
  traverse1 f (NonEmptyVector v) =
      -- Get the length of the vector in /O(1)/ time
      let !n = F.length v
      -- Use fromListN to be more efficient in construction of resulting vector
      -- Also behaves better with compact regions, preventing runtime exceptions
      in (NonEmptyVector . Vector.fromListN n . F.toList)
         <$> traverse1 f (NonEmpty.fromList $ F.toList v)
         -- notice that NonEmpty.fromList is suposedly safe since the vector is NonEmpty...

  {-# INLINE traverse1 #-}

instance FunctorWithIndex Int NonEmptyVector where
  imap f (NonEmptyVector v) = NonEmptyVector $ imap f v
instance FoldableWithIndex Int NonEmptyVector where
  ifoldMap f (NonEmptyVector v) = ifoldMap f v
instance TraversableWithIndex Int NonEmptyVector where
  itraverse f (NonEmptyVector v) = NonEmptyVector <$> itraverse f v

--------------------------------------------------------------------------------

newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable)

instance Foldable1 v    => Foldable1    (Cyclic v)

instance Traversable1 v => Traversable1 (Cyclic v) where
  traverse1 f (Cyclic v) = Cyclic <$> traverse1 f v
instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v


instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

instance HasFromFoldable1 v => HasFromFoldable1 (Cyclic v)  where
  fromFoldable1 = Cyclic . fromFoldable1
  fromNonEmpty  = Cyclic . fromNonEmpty

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

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




--------------------------------------------------------------------------------


testPoly :: SimplePolygon Point Int
testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------
