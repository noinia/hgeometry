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


  , Cyclic(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           GHC.Generics
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class
import           Geometry.Polygon.Simple.Implementation
import           Geometry.Properties

import           Data.Maybe
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV
-- import           Data.Vector.Circular (CircularVector)
-- import qualified Data.Vector.Circular as CV
-- import qualified Data.Vector.Circular.Util as CV

--------------------------------------------------------------------------------

class HasFromFoldable f where
  fromFoldable :: Foldable g => g a -> f a
  fromFoldable = fromList . F.toList

  fromList :: [a] -> f a
  {-# MINIAL fromList #-}

instance HasFromFoldable Vector.Vector  where
  fromList = Vector.fromList

--------------------------------------------------------------------------------

newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable)

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

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

--------------------------------------------------------------------------------


-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

-- | By default we store simple polygons as circular vectors.
type SimplePolygon = SimplePolygonF (Cyclic Vector.Vector)

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
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         )
      => HasOuterBoundary (SimplePolygonF f point r) where
  outerBoundary = vertices
  outerBoundaryVertexAt i = singular (vertexAt i)

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , HasFromFoldable f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         ) => Polygon_ (SimplePolygonF f) point r where
  area = areaSimplePolygon

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , HasFromFoldable f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         ) => SimplePolygon_ (SimplePolygonF f) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable



--------------------------------------------------------------------------------


testPoly :: SimplePolygon Point Int
testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------
