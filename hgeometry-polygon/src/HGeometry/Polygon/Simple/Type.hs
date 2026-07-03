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
  , PolygonDart(..)
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
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import qualified HGeometry.Foldable.Util as F
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph.Dart (Direction(..), rev)

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCW order
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


----------------------------------------

-- | Dart type that we can use for simple polygons
data PolygonDart = PolygonDart {-#UNPACK#-}!Direction {-#UNPACK#-}!Int
  deriving (Show,Eq,Ord,Generic)

instance NFData PolygonDart

instance VertexContainer f vertex => HasDarts' (SimplePolygonF f vertex) where
  -- | Positive darts are oriented "forward" along the boundary of the polygon (so CCW)
  -- negative darts are oriented backward (so CW).
  type DartIx (SimplePolygonF f vertex) = PolygonDart
  type Dart   (SimplePolygonF f vertex) = ()
    -- for now edges don't store additional data.
  dartAt u = \pUnitFUnit poly -> poly <$ indexed pUnitFUnit u ()
  numDarts = (2*) . numVertices

instance VertexContainer f vertex
         => HasDarts (SimplePolygonF f vertex) (SimplePolygonF f vertex) where
  darts = conjoined trav (itrav.indexed)
    where
      trav        :: Applicative g
                  => (() -> g ()) -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      trav f poly = let f' = WrapApplicative . f in
                    unwrapApplicative $
                      poly <$ (vertices' (\x -> x <$ f' () <* f' ()) poly)

      itrav        :: Applicative g
                   => (DartIx (SimplePolygonF f vertex) -> () -> g ())
                   -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      itrav f poly = let f' i = WrapApplicative . f i in
                     unwrapApplicative $
                       poly <$ vertices' (Indexed $ \v x ->
                                           x <$ f' (PolygonDart Negative v) ()
                                             <* f' (PolygonDart Positive v) ()
                                         ) poly

      vertices' :: IndexedTraversal1' (VertexIx (SimplePolygonF f vertex))
                                      (SimplePolygonF f vertex) vertex
      vertices' = vertices

----------------------------------------

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

----------------------------------------

instance VertexContainer f vertex => DiGraph_ (SimplePolygonF f vertex) where
  endPoints poly (PolygonDart d i) = case d of
      Negative -> ((i+n-1) `mod` n, (i+n)   `mod` n)
      Positive -> ((i+n) `mod` n,   (i+n+1) `mod` n)
    where
      n = numVertices poly

  outgoingDartsOf u = conjoined f ixf
    where
      f        :: (Contravariant g, Applicative g)
               => (() -> g ()) -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      f g poly = poly <$ g () <* g ()

      ixf         :: (Indexable PolygonDart p, Contravariant g, Applicative g)
                  => p () (g ()) -> SimplePolygonF f vertex -> g (SimplePolygonF f vertex)
      ixf pg poly = poly <$ indexed pg (PolygonDart Negative u) ()
                         <* indexed pg (PolygonDart Positive u) ()

  twinDartOf (PolygonDart d i) = to . const . Just $ PolygonDart (rev d) i

instance VertexContainer f vertex => BidirGraph_ (SimplePolygonF f vertex) where
  twinOf (PolygonDart d i) = to . const $ PolygonDart (rev d) i
  getPositiveDart _ e = PolygonDart Positive e

--------------------------------------------------------------------------------

-- | Get the underlying cyclic vector.
toCyclic :: SimplePolygonF (Cyclic v) point -> Cyclic v point
toCyclic = view _SimplePolygonF
