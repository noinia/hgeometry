{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PolyLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polyline and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.PolyLine
  ( PolyLineF(..), PolyLine
  , _PolyLineF
  , module HGeometry.PolyLine.Class
  , HasVertices(..)
  , HasEdges(..)
  ) where


import Control.DeepSeq (NFData)
import Control.Lens
import Data.Functor.Classes
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics
import HGeometry.Box
import HGeometry.Foldable.Util
import HGeometry.Interval.Class
import HGeometry.Point
import HGeometry.PolyLine.Class
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector.NonEmpty.Util ()
import Hiraffe.Graph

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype PolyLineF f point = PolyLine (f point)
  deriving stock (Generic,Show)
  deriving newtype (NFData,Functor,Foldable,Foldable1,Eq,Ord,Eq1,Ord1)

-- | By default we store simple poylline as non-empty vectors.
type PolyLine = PolyLineF NonEmptyVector

type instance Dimension (PolyLineF f point) = Dimension point
type instance NumType   (PolyLineF f point) = NumType point


-- | Access the container
_PolyLineF :: Iso (PolyLineF f point) (PolyLineF f' point')
                  (f point)                (f' point' )
_PolyLineF = iso (\(PolyLine vs) -> vs) PolyLine

instance Traversable f => Traversable (PolyLineF f) where
  traverse f (PolyLine vs) = PolyLine <$> traverse f vs
instance Traversable1 f => Traversable1 (PolyLineF f) where
  traverse1 f (PolyLine vs) = PolyLine <$> traverse1 f vs

instance ( TraversableWithIndex Int f
         , Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         ) => HasVertices (PolyLineF f point) (PolyLineF f point') where
  vertices = _PolyLineF . traversed1

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         ) => HasPoints (PolyLineF f point) (PolyLineF f point') point point' where
  allPoints = _PolyLineF . traversed1

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         , DefaultTransformByConstraints (PolyLineF f point) 2 r
         , Point_ point 2 r
         ) => IsTransformable (PolyLineF f point)

instance ( Traversable1 f
         , IxValue (f point) ~ point
         , Index   (f point) ~ Int
         , Ixed    (f point)
         , Point_ point 2 r
         -- , OptVector_ 2 r, OptMetric_ 2 r
         ) => IsBoxable (PolyLineF f point)

instance ( TraversableWithIndex Int f
         , Traversable1 f
         , Ixed (f point)
         , IxValue (f point) ~ point
         , Index (f point) ~ Int
         ) => HasVertices' (PolyLineF f point) where
  type Vertex   (PolyLineF f point) = point
  type VertexIx (PolyLineF f point) = Int
  vertexAt i = _PolyLineF . iix i

instance ( Traversable1 f
         -- , Ixed (f point)
         -- , IxValue (f point) ~ point
         -- , Index (f point) ~ Int
         ) => HasStart (PolyLineF f point) point where
  start = head1

instance ( Traversable1 f
         -- , Ixed (f point)
         -- , IxValue (f point) ~ point
         -- , Index (f point) ~ Int
         ) => HasEnd (PolyLineF f point) point where
  end = last1

instance ( Traversable1 f
         , IxValue (f point) ~ point, Index (f point) ~ Int
         , Ixed (f point)
         , HasFromFoldable1 f
         , Point_ point d r
         , TraversableWithIndex Int f
         ) => PolyLine_ (PolyLineF f point) point where

instance ( Traversable1 f
         , IxValue (f point) ~ point, Index (f point) ~ Int
         , Ixed (f point)
         , HasFromFoldable1 f
         , Point_ point d r
         , TraversableWithIndex Int f
         ) => ConstructablePolyLine_ (PolyLineF f point) point where
  polyLineFromPoints = PolyLine . fromFoldable1
