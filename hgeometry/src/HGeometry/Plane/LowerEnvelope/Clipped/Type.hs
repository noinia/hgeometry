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
  , ClippedMDCell, ClippedMDCell'
  , ClippedMDCell''(..)
  ) where

import           Control.Lens hiding (IsEmpty, IsNonEmpty)
import           Control.Subcategory.Functor
import           Data.Bifunctor
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import           HGeometry.Intersection
import           HGeometry.Plane.LowerEnvelope.Connected.Region
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Triangle


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
  ClippedMinimizationDiagram (NEMap plane (ClippedMDCell r plane ()))

-- TODO: expose the a type instead of ()

deriving instance (Show r, Num r, Show plane) => Show (ClippedMinimizationDiagram' r plane)

type instance NumType   (ClippedMinimizationDiagram' r plane) = r
type instance Dimension (ClippedMinimizationDiagram' r plane) = 2

-- | Get access to the underlying NonEmpty Map
_ClippedMinimizationDiagramMap :: (NumType plane ~ r)
                               => Iso' (ClippedMinimizationDiagram plane)
                                       (NEMap plane (ClippedMDCell r plane ()))
_ClippedMinimizationDiagramMap = coerced
{-# INLINE _ClippedMinimizationDiagramMap #-}

instance Constrained (ClippedMinimizationDiagram' r) where
  type Dom (ClippedMinimizationDiagram' r) plane = Ord plane

instance CFunctor (ClippedMinimizationDiagram' r) where
  cmap f (ClippedMinimizationDiagram m) = ClippedMinimizationDiagram $
    NEMap.foldMapWithKey (\plane cell -> NEMap.singleton (f plane) (mapPlane f cell)) m

--------------------------------------------------------------------------------
-- * Representing Cells in a Clipped MinimizationDiagram

-- | Cells in the Minimization diagram (i.e. the projected lower envelope of planes)
-- parameterized by the numeric type and the planes
type ClippedMDCell r plane a = ClippedMDCell' r (MDVertex r plane a)


-- | Map the plane to some other type
mapPlane   :: (plane -> plane') -> ClippedMDCell r plane a -> ClippedMDCell r plane' a
mapPlane f = first (first f)


-- | We mostly use ClippedMDCell'''s where the Extra is just a Point
type ClippedMDCell' r vertex = ClippedMDCell'' r vertex (Point 2 r)

-- | Helper type for representing cells in a minimzation diagram. These cells are possibly
-- degenerate convex polygons, whose vertices are either of type 'vertex' or of type
-- 'Point 2 r'.
newtype ClippedMDCell'' r vertex extra = ClippedMDCell
  (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex extra)
                                   (ClippedBoundedRegion r vertex extra))

type instance NumType   (ClippedMDCell'' r vertex extra) = r
type instance Dimension (ClippedMDCell'' r vertex extra) = 2

deriving instance (Show vertex, Point_ vertex 2 r, Point_ extra  2 r, Show extra, Show r
                  ) => Show (ClippedMDCell'' r vertex extra)
deriving instance (Eq vertex, Eq r, Eq extra
                  ) => Eq   (ClippedMDCell'' r vertex extra)


_ClippedMDCell :: Iso (ClippedMDCell'' r vertex extra) (ClippedMDCell'' r' vertex' extra')
                      (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex extra)
                                                       (ClippedBoundedRegion r vertex extra))
                       (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex' extra')
                                                        (ClippedBoundedRegion r' vertex' extra'))
_ClippedMDCell = coerced


instance Functor (ClippedMDCell'' r vertex) where
  fmap = second

instance Bifunctor (ClippedMDCell'' r) where
  bimap f g (ClippedMDCell poly) = ClippedMDCell $ bimap (bimap f g)
                                                         (fmap (bimap f g)) poly

--
-- instance HasVertices' (ClippedMDCell'' r vertex extra) where
--   type VertexIx (ClippedMDCell'' r vertex extra) =
--     VertexIx (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex extra)
--                                               (ClippedBoundedRegion r vertex extra))
--   type Vertex (ClippedMDCell'' r vertex extra) =
--     Vertex (PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex extra)
--                                             (ClippedBoundedRegion r vertex extra))
--   vertexAt u = _ClippedMDCell.vertexAt u

-- instance HasVertices (ClippedMDCell'' r vertex extra) (ClippedMDCell'' r' vertex' extra') where
--   vertices = _ClippedMDCell.vertices

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

-- | Intersecting Rectangle and Region in a MinimizationDiagram yields a clipped cell.
type instance Intersection (Rectangle corner) (Region r vertex) = Maybe (ClippedMDCell' r vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Rectangle corner `HasIntersectionWith` (Region r vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Rectangle corner `IsIntersectableWith` (Region r vertex) where
  rect `intersect` reg = ClippedMDCell <$> case reg of
    BoundedRegion   convex -> rect `intersect` convex
    UnboundedRegion convex -> rect `intersect` convex
