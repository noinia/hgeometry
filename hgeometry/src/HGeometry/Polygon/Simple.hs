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
  , toCyclic
  , VertexContainer
  , HasInPolygon(..)
  , inSimplePolygon
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Functor.Classes
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics
import           HGeometry.Boundary
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import           HGeometry.Intersection
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Vector.NonEmpty.Util ()

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

-- | shortcut for all default properties of f we need to store the vertices.
type VertexContainer f point = ( IxValue (f point) ~ point
                               , Index (f point) ~ Int
                               , TraversableWithIndex Int f
                               , Traversable1 f
                               , Ixed (f point)
                               , HasDirectedTraversals f
                               )

instance ( VertexContainer f point
         ) => HasVertices (SimplePolygonF f point) (SimplePolygonF f point') where
  vertices = _SimplePolygonF . traversed1

instance ( VertexContainer f point
         ) => HasPoints (SimplePolygonF f point) (SimplePolygonF f point') point point' where
  allPoints = _SimplePolygonF . traversed1

instance ( VertexContainer f point
         , DefaultTransformByConstraints (SimplePolygonF f point) 2 r
         , Point_ point 2 r
         ) => IsTransformable (SimplePolygonF f point)

instance ( VertexContainer f point
         , Point_ point 2 r
         ) => IsBoxable (SimplePolygonF f point)

instance ( VertexContainer f point
         ) => HasVertices' (SimplePolygonF f point) where
  type Vertex   (SimplePolygonF f point) = point
  type VertexIx (SimplePolygonF f point) = Int
  vertexAt i = _SimplePolygonF . iix i
  numVertices = F.length . view _SimplePolygonF

instance ( VertexContainer f point
         ) => HasOuterBoundary (SimplePolygonF f point) where
  outerBoundary = _SimplePolygonF . traversed1
  outerBoundaryVertexAt i = singular (vertexAt i)

  ccwOuterBoundaryFrom i = _SimplePolygonF.traverseRightFrom i
  cwOuterBoundaryFrom  i = _SimplePolygonF.traverseLeftFrom  i

instance ( Point_ point 2 r
         , HasFromFoldable1 f
         , VertexContainer f point
         ) => Polygon_ (SimplePolygonF f point) point r where
  area = areaSimplePolygon
  ccwPredecessorOf u = singular $ vertexAt (pred u)
  ccwSuccessorOf   u = singular $ vertexAt (succ u)

instance ( Point_ point 2 r
         , VertexContainer f point
         , HasFromFoldable1 f
         ) => SimplePolygon_ (SimplePolygonF f point) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable1
                         . NonEmpty.fromList . F.toList

  fromPoints = Just
             . toCounterClockwiseOrder
             . uncheckedFromCCWPoints
             . requireThree "fromPoints" . F.toList
  -- TODO: verify that:
  --      we have no repeated vertices,
  --      no self intersections, and
  --      not all vertices are colinear


-- | Validate that we have at least three points
requireThree :: String -> [a] -> [a]
requireThree _ lst@(_:_:_:_) = lst
requireThree label _ = error $
  "HGeometry.Polygon." ++ label ++ ": Polygons must have at least three points."


instance ( Show point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Show (SimplePolygonF f point) where
  showsPrec = showsPrecSimplePolygon "SimplePolygon"

instance ( Read point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Read (SimplePolygonF f point) where
  readsPrec = readsPrecSimplePolygon "SimplePolygon"


{-
instance (SimplePolygon_ (SimplePolygonF f) point r, Fractional r, Ord r)
         => HasSquaredEuclideanDistance (SimplePolygonF f point) where
  pointClosestToWithDistance = pointClosestToWithDistanceSimplePolygon
-}

--------------------------------------------------------------------------------

-- | Get the underlying cyclic vector.
toCyclic :: SimplePolygonF (Cyclic v) point -> Cyclic v point
toCyclic = view _SimplePolygonF

--------------------------------------------------------------------------------

_testPoly :: SimplePolygon (Point 2 Int)
_testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------

instance SimplePolygon_ (SimplePolygonF f point) point r
         => HasInPolygon (SimplePolygonF f point) point r

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => HasIntersectionWith (Point 2 r) (SimplePolygonF f point) where
  q `intersects` pg = q `inSimplePolygon` pg /= StrictlyOutside

type instance Intersection (Point 2 r) (SimplePolygonF f point) = Maybe (Point 2 r)

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => IsIntersectableWith (Point 2 r) (SimplePolygonF f point) where
  q `intersect` pg | q `intersects` pg = Just q
                   | otherwise         = Nothing
  -- this implementation is a bit silly but ok
