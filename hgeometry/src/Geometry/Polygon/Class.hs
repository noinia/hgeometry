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
module Geometry.Polygon.Class
  ( SimplePolygon_(..)

  ) where

import qualified Data.Foldable as F
import           Control.Lens
import           Data.Coerce
import           GHC.Generics

import           Geometry.Point

import           Data.Maybe
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Circular.Util as CV


--------------------------------------------------------------------------------

class HasVertices graph graph' where
  type Vertex   graph
  type VertexIx graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')


class HasEdges graph where
  type Edge   graph
  type EdgeIx graph
  edges :: IndexedTraversal' (EdgeIx graph) graph (Edge graph)

--------------------------------------------------------------------------------

class ( HasVertices (polygon point r) (polygon point r)
      , Vertex      (polygon point r) ~ point 2 r
      , Point_ point 2 r
      , VertexIx    (polygon point r) ~ Int
      ) => SimplePolygon_ polygon point r where

  -- | given the vertices of the polygon, in CCW order, constructs the
  -- polygon. The vertices are numbered in the order they are given.
  --
  -- pre: - the vertices are given in CCW order
  --      - at least 3 vertices, not all colinear
  --      - no repeated vertices
  --      - no self-inttersections
  --
  -- running time: \(O(n)\)
  uncheckedFromPoints :: Foldable f => f (point 2 r) -> polygon point r


--------------------------------------------------------------------------------

newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

type SimplePolygon point r = SimplePolygonF CircularVector


deriving instance Eq (f (point 2 r)) => Eq (SimplePolygonF f point r)

instance Wrapped   (SimplePolygonF f point r)
instance Rewrapped (SimplePolygonF f point r) (f (point 2 r))

_SimplePolygonF :: Iso (SimplePolygonF f point r) (SimplePolygonF f' point' r')
                       (f (point 2 r))              (f' (point' 2 r'))
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon


instance TraversableWithIndex Int f
      => HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r') where
  type Vertex   (SimplePolygonF f point r) = point 2 r
  type VertexIx (SimplePolygonF f point r) = Int
  vertices = _SimplePolygonF . itraversed

instance HasVertices (SimplePolygon point r) (SimplePolygon point' r') where
  type Vertex   (SimplePolygonF f point r) = point 2 r
  type VertexIx (SimplePolygonF f point r) = Int
  vertices = _SimplePolygonF . CV.itraversedRight

instance ( Point_ point 2 r
         ) => SimplePolygon_ (SimplePolygonF CircularVector) point r where
  uncheckedFromPoints = MkSimplePolygon . fromMaybe err . CV.fromList . F.toList
    where
      err = error "uncheckedFromPoints, circular vector: no vertices found!"

testPoly :: SimplePolygonF [] Point Int
testPoly = MkSimplePolygon [Point2 10 20, origin]
