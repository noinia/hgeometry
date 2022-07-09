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
import qualified Data.Foldable as F
import           GHC.Generics
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class

import           Geometry.Point

import           Data.Maybe
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Circular.Util as CV

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

-- | By default we store simple polygons as circular vectors.
type SimplePolygon = SimplePolygonF CircularVector

deriving instance Eq (f (point 2 r)) => Eq (SimplePolygonF f point r)

-- instance Wrapped   (SimplePolygonF f point r)
-- instance Rewrapped (SimplePolygonF f point r) (f (point 2 r))

-- | Access the container
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
  uncheckedFromCCWPoints = MkSimplePolygon . fromMaybe err . CV.fromList . F.toList
    where
      err = error "uncheckedFromCCWPoints, circular vector: no vertices found!"


testPoly :: SimplePolygonF [] Point Int
testPoly = MkSimplePolygon [Point2 10 20, origin]



--------------------------------------------------------------------------------

newtype ConvexPolygonF f point r = MkConvexPolygon (SimplePolygonF f point r)
  deriving newtype (Eq,SimplePolygon_)
