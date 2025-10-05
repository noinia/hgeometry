{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.PossiblyDegenerate
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Covnex polygon that may be degenerate, i.e. may also be a line segment or point.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.PossiblyDegenerate
  ( PossiblyDegenerateSimplePolygon(..)
  , HalfPlane_x_SimplePolygon_Component
  ) where

import Control.DeepSeq
import Control.Lens
import HGeometry.Ext
import HGeometry.HalfSpace
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Polygon.Simple.Type
import HGeometry.Properties
import Data.Bifoldable1
import Data.Foldable1
import Data.Bifoldable
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A simple polygon (or a "subtype" thereof) that may be degenerate; i.e. may be a single
-- vertex, or a point.
data PossiblyDegenerateSimplePolygon vertex polygon =
    DegenerateVertex vertex
  | DegenerateEdge (ClosedLineSegment vertex)
  | ActualPolygon polygon
  deriving (Show,Eq,Functor,Generic)

instance (NFData vertex, NFData polygon
         ) => NFData (PossiblyDegenerateSimplePolygon vertex polygon)

instance Bifunctor PossiblyDegenerateSimplePolygon where
  bimap f g = \case
    DegenerateVertex v -> DegenerateVertex (f v)
    DegenerateEdge e   -> DegenerateEdge (fmap f e)
    ActualPolygon poly -> ActualPolygon (g poly)


instance Bifoldable1 PossiblyDegenerateSimplePolygon where
  bifoldMap1 f g = \case
    DegenerateVertex v -> f v
    DegenerateEdge e   -> foldMap1 f e
    ActualPolygon poly -> g poly

instance Bifoldable PossiblyDegenerateSimplePolygon where
  bifoldMap = bifoldMap1

type instance NumType   (PossiblyDegenerateSimplePolygon vertex polygon) = NumType vertex
type instance Dimension (PossiblyDegenerateSimplePolygon vertex polygon) = 2

--------------------------------------------------------------------------------

-- | A HalfPlane and a simple polygon intersect in a bunch of components, each of
-- which is a possiblyDegenerate simple polygon.
type instance Intersection (HalfSpaceF line) (SimplePolygonF f point) =
  [HalfPlane_x_SimplePolygon_Component f (NumType point) point]
-- | A single Component of a HalfPlane x SimplePolygon intersection.

type HalfPlane_x_SimplePolygon_Component f r vertex =
  PossiblyDegenerateSimplePolygon vertex (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))

-- | If we drag along extra information in the halfplane polygon intersection we lose it
type instance Intersection (HalfSpaceF line :+ extra) (SimplePolygonF f point :+ extra') =
  Intersection (HalfSpaceF line) (SimplePolygonF f point)
