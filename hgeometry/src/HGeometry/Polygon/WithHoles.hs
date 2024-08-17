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
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens hiding (holes)
import qualified Data.Foldable as F
import           Data.Functor.Classes
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Semigroup.Foldable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics
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
data PolygonalDomainF h f point =
  PolygonalDomain (SimplePolygonF f point) -- ^ the outer boundary
                  (h (SimplePolygonF f point)) -- ^ the holes
  deriving stock (Generic)

-- | Polygonal domain implemented using Vectors
type PolygonalDomain = PolygonalDomainF Vector (Cyclic NonEmptyVector)



-- class Polygon_ polygon point r => HasHoles polygon where
--   type HoleIx polygon
--   -- ^ Traversal over the holes in the polygon
--   holes :: IndexedTraversal (HoleIx polygon) polygon (SimplePolygon point)
