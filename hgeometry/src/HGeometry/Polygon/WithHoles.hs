{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.WithHoles
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A simple type for representing polygonswith holes
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.WithHoles
  ( PolygonalDomainF(PolygonalDomain)
  , PolygonalDomain
  , asSimplePolygon
  , outerBoundaryPolygon
  , theHoles
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens hiding (holes)
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>), MaybeApply(..))
import           Data.Functor.Classes
import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics (Generic)
import           HGeometry.Boundary
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import           HGeometry.Intersection
import           HGeometry.LineSegment.Intersection.BentleyOttmann
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Simple data type modelling polygons with holes
data PolygonalDomainF (h     :: Type -> Type)
                      (f     :: Type -> Type)
                      (point :: Type) =
    PolygonalDomain (SimplePolygonF f point) -- ^ the outer boundary
                    (h (SimplePolygonF f point)) -- ^ the holes
  deriving stock (Generic)

instance ( NFData    (SimplePolygonF f point)
         , NFData (h (SimplePolygonF f point))
         ) => NFData (PolygonalDomainF h f point)

deriving instance ( Show    (SimplePolygonF f point)
                  , Show (h (SimplePolygonF f point))
                  ) => Show (PolygonalDomainF h f point)
deriving instance ( Read    (SimplePolygonF f point)
                  , Read (h (SimplePolygonF f point))
                  ) => Read (PolygonalDomainF h f point)
deriving instance ( Eq    (SimplePolygonF f point)
                  , Eq (h (SimplePolygonF f point))
                  ) => Eq (PolygonalDomainF h f point)

-- | The simple polygon representing the outer boundary
outerBoundaryPolygon :: Lens' (PolygonalDomainF h f point) (SimplePolygonF f point)
outerBoundaryPolygon = lens (\(PolygonalDomain outer _)    -> outer)
                            (\(PolygonalDomain _ hs) outer -> PolygonalDomain outer hs)

-- | Lens to access the holes
theHoles :: Lens (PolygonalDomainF h f point) (PolygonalDomainF h' f point)
                 (h (SimplePolygonF f point)) (h' (SimplePolygonF f point))
theHoles = lens (\(PolygonalDomain _     hs)    -> hs)
                (\(PolygonalDomain outer _)  hs -> PolygonalDomain outer hs)

-- | Polygonal domain implemented using Vectors
type PolygonalDomain point = PolygonalDomainF Vector (Cyclic NonEmptyVector) point

type instance Dimension (PolygonalDomainF h f point) = 2
type instance NumType   (PolygonalDomainF h f point) = NumType point

instance (Functor h, Functor f) => Functor (PolygonalDomainF h f) where
  fmap f (PolygonalDomain outer hs) =
    PolygonalDomain (fmap f outer) (fmap (fmap f) hs)

instance (Foldable h, Foldable f) => Foldable (PolygonalDomainF h f) where
  foldMap f (PolygonalDomain outer hs) = foldMap f outer <> foldMap (foldMap f) hs

instance (Traversable h, Traversable f) => Traversable (PolygonalDomainF h f) where
  traverse f (PolygonalDomain outer hs) =
    PolygonalDomain <$> traverse f outer <*> traverse (traverse f) hs

instance (Foldable h, Foldable1 f) => Foldable1 (PolygonalDomainF h f) where
  foldMap1 f (PolygonalDomain outer hs) =
      foldMap1 f outer `combine` foldMap (Just . foldMap1 f) hs
    where
      combine x0 = maybe x0 (x0 <>)

instance (Traversable h, Traversable1 f) => Traversable1 (PolygonalDomainF h f) where
  traverse1 f (PolygonalDomain outer hs) =
    PolygonalDomain <$> traverse1 f outer <.*> traverse1Maybe (traverse1 f) hs

instance ( VertexContainer f point, Traversable h
         ) => HasPoints (PolygonalDomainF h f point)
                        (PolygonalDomainF h f point') point point' where
  allPoints = traversed1

instance ( VertexContainer f point, Traversable h
         , DefaultTransformByConstraints (PolygonalDomainF h f point) 2 r
         , Point_ point 2 r
         ) => IsTransformable (PolygonalDomainF h f point)

instance ( VertexContainer f point
         , Point_ point 2 r
         ) => IsBoxable (PolygonalDomainF h  f point) where
  boundingBox = boundingBox . view outerBoundaryPolygon


instance ( HoleContainer h f point, VertexContainer f point
         ) => HasVertices (PolygonalDomainF h f point) (PolygonalDomainF h f point') where
  vertices = conjoined traverse1 (itraverse1 . indexed)
    where
      itraverse1 f (PolygonalDomain outer hs) =
        PolygonalDomain <$>  itraverseOf (reindexed Outer vertices) f outer
                        <.*> itraverseOf (reindexed inner $ itraversed <.> vertices)
                                         (\i -> MaybeApply . Left . f i)
                                         hs

-- | Containers that stores holes must satisfy the following constraints:
type HoleContainer h f point =
  ( TraversableWithIndex Int h
  , Index   (h (SimplePolygonF f point)) ~ Int
  , IxValue (h (SimplePolygonF f point)) ~ SimplePolygonF f point
  , Ixed    (h (SimplePolygonF f point))
  )

instance ( HoleContainer h f point
         ) => HasHoles (PolygonalDomainF h f point) where
  type HoleIx (PolygonalDomainF h f point) = Int
  type HoleF  (PolygonalDomainF h f point) = f

  holes    = theHoles .> itraversed
  holeAt i = theHoles .> iix i

-- | The indices we use to identify vertices
data VtxIx = Outer {-#UNPACK#-}!Int
           | Inner {-#UNPACK#-}!Int {-#UNPACK#-}!Int
           deriving (Show,Read,Eq,Ord)

-- | Construct an index of an inner vertex
inner :: (Int,Int) -> VtxIx
inner = uncurry Inner

instance ( HoleContainer h f point
         , VertexContainer f point
         ) => HasVertices' (PolygonalDomainF h f point) where
  type Vertex   (PolygonalDomainF h f point) = point
  type VertexIx (PolygonalDomainF h f point) = VtxIx

  vertexAt = \case
    Outer   j -> reindexed Outer $ outerBoundaryPolygon .> vertexAt j
    Inner i j -> reindexed inner $ holeAt i <.> vertexAt j

  numVertices pg = numVertices (pg^.outerBoundaryPolygon) + sumOf (holes.to numVertices) pg

instance ( HoleContainer h f point, VertexContainer f point
         ) => HasOuterBoundary (PolygonalDomainF h f point) where
  outerBoundary        = reindexed Outer $ outerBoundaryPolygon .> outerBoundary
  ccwOuterBoundaryFrom = \case
    Outer u -> reindexed Outer $ outerBoundaryPolygon .> ccwOuterBoundaryFrom u
    u       -> error' "ccwOuterBoundaryFrom" u

  cwOuterBoundaryFrom  = \case
    Outer u -> reindexed Outer $ outerBoundaryPolygon .> cwOuterBoundaryFrom u
    u       -> error' "cwOuterBoundaryFrom" u

  outerBoundaryVertexAt = \case
    Outer u -> reindexed Outer $ outerBoundaryPolygon .> outerBoundaryVertexAt u
    u       -> error' "outerBoundaryVertexAt" u

  outerBoundaryEdges  = reindexed mapEdge $ outerBoundaryPolygon .> outerBoundaryEdges
  outerBoundaryEdgeAt = \case
    Outer u -> reindexed mapEdge $ outerBoundaryPolygon .> outerBoundaryEdgeAt u
    u       -> error' "outerBoundaryVertexAt" u

-- | Helper to produce a more useful error message
error'     :: String -> VtxIx -> a
error' s u = error $ s <> ", vertex " <> show u <> " not on outer boundary"

-- | remap an edge on the outer boundary
mapEdge       :: (Int,Int) -> (VtxIx,VtxIx)
mapEdge (u,v) = (Outer u, Outer v)

instance ( Point_ point 2 r
         , HasFromFoldable1 f
         , VertexContainer f point
         , HoleContainer h f point
         ) => Polygon_ (PolygonalDomainF h f point) point r where
  extremes u = extremes u . view outerBoundaryPolygon
  ccwPredecessorOf = \case
    Outer u   -> reindexed Outer     $ outerBoundaryPolygon .> ccwPredecessorOf u
    Inner i j -> reindexed (Inner i) $ singular (holeAt i)  .> ccwPredecessorOf j
  ccwSuccessorOf = \case
    Outer u   -> reindexed Outer     $ outerBoundaryPolygon .> ccwSuccessorOf u
    Inner i j -> reindexed (Inner i) $ singular (holeAt i)  .> ccwSuccessorOf j

{-

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , HolesContainer h f point
         )
         => HasInPolygon (PolygonalDomainF h f point) point r where
  inPolygon q pg = case q `inPolygon` (pg^.outerBoundaryPolygon) of
                     Inside  -> case getResult $ ifoldMapOf holes (testInHole q) pg of
                       Outside -> Inside
                       res     -> res
                     res     -> Outer <$> res
    where
      testInHole q i pg = Intersect $ Inner i <$> q `inPolygon` (pg^.outerBoundaryPolygon)


newtype Intersect = Intersect { getResult :: PointLocationResultWith Vtx }

instance Semigroup Intersect where
  (Intersect Outside) <> r = r
  (Intersect Inside)


instance Semigroup Monoid where
  mempty = Intersect Outside

-}

--------------------------------------------------------------------------------


-- | interpret a simple polygon as a Polygonal domain.
asSimplePolygon :: (HasFromFoldable h, HoleContainer h f point)
                 => Prism' (PolygonalDomainF h f point) (SimplePolygonF f point)
asSimplePolygon = prism' (flip PolygonalDomain (fromList []))
                         (\pd -> if nullOf holes pd then Just (pd^.outerBoundaryPolygon)
                                 else Nothing
                         )
