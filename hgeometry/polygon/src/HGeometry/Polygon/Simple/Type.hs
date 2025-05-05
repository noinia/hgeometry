{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.Type
  ( SimplePolygon
  , SimplePolygonF(..)
  , toCyclic
  , VertexContainer
  , _SimplePolygonF
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import           Data.Functor.Apply (WrappedApplicative(..))
import           Data.Functor.Classes
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import qualified HGeometry.Foldable.Util as F
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph.Class

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point = MkSimplePolygon (f point)
  deriving stock (Generic)
  deriving newtype (NFData,Functor,Foldable,Foldable1,Eq,Ord,Eq1,Ord1)


-- | By default we store simple polygons as non-empty circular vectors.
type SimplePolygon = SimplePolygonF (Cyclic NonEmptyVector)

type instance Dimension (SimplePolygonF f point) = 2
type instance NumType   (SimplePolygonF f point) = NumType point

-- TODO: should we use allow cyclic shifts?
-- deriving instance Eq (f point)  => Eq (SimplePolygonF f point)
-- deriving instance Ord (f point) => Ord (SimplePolygonF f point)


-- | Access the container
_SimplePolygonF :: Iso (SimplePolygonF f point) (SimplePolygonF f' point')
                       (f point)                (f' point' )
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon

instance Traversable f => Traversable (SimplePolygonF f) where
  traverse f (MkSimplePolygon vs) = MkSimplePolygon <$> traverse f vs
instance Traversable1 f => Traversable1 (SimplePolygonF f) where
  traverse1 f (MkSimplePolygon vs) = MkSimplePolygon <$> traverse1 f vs

instance (ShiftedEq (f point), ElemCyclic (f point) ~ point
         ) => ShiftedEq (SimplePolygonF f point) where
  type ElemCyclic (SimplePolygonF f point) = point
  isShiftOf p q = isShiftOf (p^._SimplePolygonF) (q^._SimplePolygonF)

instance (Foldable f, ToJSON point) => ToJSON (SimplePolygonF f point) where
  toJSON pg =  object [ "tag"           Aeson..= ("SimplePolygon" :: String)
                      , "vertices"      Aeson..= F.toList pg
                      ]
instance (HasFromFoldable1 f, FromJSON point) => FromJSON (SimplePolygonF f point) where
  parseJSON = withObject "SimplePolygon" $ \o -> do
                ("SimplePolygon" :: String) <- o .: "tag"
                MkSimplePolygon . F.fromNonEmpty @f @point <$> o .: "vertices"

-- | shortcut for all default properties of f we need to store the vertices.
type VertexContainer f point = ( IxValue (f point) ~ point
                               , Index (f point) ~ Int
                               , TraversableWithIndex Int f
                               , Traversable1 f
                               , Ixed (f point)
                               , HasDirectedTraversals f
                               )

instance ( VertexContainer f point
         ) => HasPoints (SimplePolygonF f point) (SimplePolygonF f point') point point' where
  allPoints = _SimplePolygonF . traversed1

instance ( VertexContainer f point
         , Point_ point 2 r
         ) => IsBoxable (SimplePolygonF f point)

instance ( VertexContainer f point
         ) => HasVertices (SimplePolygonF f point) (SimplePolygonF f point') where
  vertices = _SimplePolygonF . traversed1

instance ( VertexContainer f point
         ) => HasVertices' (SimplePolygonF f point) where
  type Vertex   (SimplePolygonF f point) = point
  type VertexIx (SimplePolygonF f point) = Int
  vertexAt i = _SimplePolygonF . iix i
  numVertices = F.length . view _SimplePolygonF

instance VertexContainer f vertex => HasEdges' (SimplePolygonF f vertex) where
  type Edge   (SimplePolygonF f vertex) = ()
  type EdgeIx (SimplePolygonF f vertex) = VertexIx (SimplePolygonF f vertex)
  edgeAt u = \pUnitFUnit poly -> poly <$ indexed pUnitFUnit u ()
  -- unclear whether we should use conjoined here.
  numEdges = numVertices

instance VertexContainer f vertex
         => HasEdges (SimplePolygonF f vertex) (SimplePolygonF f vertex) where
  edges = conjoined trav (itrav.indexed)
    where
      trav        :: Applicative g
                  => (() -> g ()) -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      trav f poly = unwrapApplicative $
                    poly <$ (vertices' (\x -> x <$ WrapApplicative (f ())) poly)

      itrav        :: Applicative g
                   => (VertexIx (SimplePolygonF f vertex) -> () -> g ())
                   -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      itrav f poly = unwrapApplicative $
                     poly <$ vertices' (Indexed $ \v x -> x <$ WrapApplicative (f v ())) poly

      vertices' :: IndexedTraversal1' (VertexIx (SimplePolygonF f vertex))
                                      (SimplePolygonF f vertex) vertex
      vertices' = vertices

--------------------------------------------------------------------------------

-- | Get the underlying cyclic vector.
toCyclic :: SimplePolygonF (Cyclic v) point -> Cyclic v point
toCyclic = view _SimplePolygonF
