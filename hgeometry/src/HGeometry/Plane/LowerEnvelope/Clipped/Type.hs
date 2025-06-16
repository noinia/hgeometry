{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Clipped.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes inside a given triangle. This means
-- now all regions are actually bounded.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Clipped.Type
  ( ClippedMinimizationDiagram
  , ClippedMinimizationDiagram'
  , _ClippedMinimizationDiagramMap
  , ClippedMDCell
  , ClippedMDCell'(..)
  ) where

import           Control.Lens hiding (IsEmpty, IsNonEmpty)
import           Control.Subcategory.Functor
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
import           Data.Foldable1 as F1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NEMap, pattern IsEmpty, pattern IsNonEmpty)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.Plane.LowerEnvelope.Connected.Region
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Triangle
import           HGeometry.Vector
import           Hiraffe.Graph.Class


--------------------------------------------------------------------------------
-- * Representing (The minimization diagram of) the Lower envelope

-- | The representing (the minimization diagram) of the lower envelope of a set of planes,
-- clipped to some bounding triangle.
--
-- A 'ClippedMinimizationDiagram' is essentially just a non-empty Map mapping planes
-- to cells.
type ClippedMinimizationDiagram plane  = ClippedMinimizationDiagram' (NumType plane) plane

-- | Implementatino of the ClippedMinimizationDiagram type; r is the numeric type of the
-- planes
newtype ClippedMinimizationDiagram' r plane =
  ClippedMinimizationDiagram (NEMap plane (ClippedMDCell r plane))

deriving instance (Show r, Num r, Show plane) => Show (ClippedMinimizationDiagram' r plane)

type instance NumType   (ClippedMinimizationDiagram' r plane) = r
type instance Dimension (ClippedMinimizationDiagram' r plane) = 2

-- | Get access to the underlying NonEmpty Map
_ClippedMinimizationDiagramMap :: (NumType plane ~ r)
                               => Iso' (ClippedMinimizationDiagram plane)
                                       (NEMap plane (ClippedMDCell r plane))
_ClippedMinimizationDiagramMap = coerced


instance Constrained (ClippedMinimizationDiagram' r) where
  type Dom (ClippedMinimizationDiagram' r) plane = Ord plane

instance CFunctor (ClippedMinimizationDiagram' r) where
  cmap f (ClippedMinimizationDiagram m) = ClippedMinimizationDiagram $
    NEMap.foldMapWithKey (\plane cell -> NEMap.singleton (f plane) (fmap f <$> cell)) m

--------------------------------------------------------------------------------
-- * Representing Cells in a Clipped MinimizationDiagram

-- | Cells in the Minimization diagram (i.e. the projected lower envelope of planes)
-- parameterized by the numeric type and the planes
type ClippedMDCell r plane = ClippedMDCell' r (MDVertex r plane)

-- | Helper type for representing cells in a minimzation diagram. These cells are possibly
-- degenerate convex polygons, whose vertices are either of type 'vertex' or of type
-- 'Point 2 r'.
newtype ClippedMDCell' r vertex = ClippedMDCell
  (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex (Point 2 r))
                                   (ClippedBoundedRegion r vertex (Point 2 r)))

type instance NumType   (ClippedMDCell' r vertex) = r
type instance Dimension (ClippedMDCell' r vertex) = 2

deriving instance (Show vertex, Point_ vertex 2 r, Show r) => Show (ClippedMDCell' r vertex)
deriving instance (Eq vertex, Eq r)     => Eq (ClippedMDCell' r vertex)

instance Functor (ClippedMDCell' r) where
  fmap f (ClippedMDCell poly) = ClippedMDCell $ bimap (first f) (fmap (first f)) poly

--------------------------------------------------------------------------------

-- | Intersecting Triangles and Region in a MinimizationDiagram yields a clipped cell.
type instance Intersection (Triangle corner) (Region r vertex) = Maybe (ClippedMDCell' r vertex)


instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (Region r vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (Region r vertex) where


  tri `intersect` reg = ClippedMDCell <$> case reg of
    BoundedRegion   convex -> tri `intersect` convex
    UnboundedRegion convex -> tri `intersect` convex
