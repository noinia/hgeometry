{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Type
  ( MinimizationDiagram(..)
  , asMap
  , Region, RegionF(..)
  , toConvexPolygonIn

  , mapVertices

  --   LowerEnvelope'(LowerEnvelope)
  -- , theUnboundedVertex, boundedVertices
  -- , traverseLowerEnvelope

  -- , singleton

  -- , BoundedVertex
  -- , BoundedVertexF(Vertex)
  -- , location, definers, location2

  -- , UnboundedVertex(UnboundedVertex)
  -- , unboundedVertexId
  -- , HasUnboundedEdges(..)

  -- , EdgeGeometry
  -- , projectedEdgeGeometries, projectedEdgeGeometry


  -- , outgoingUnboundedEdge
  -- , edgeIntersectionLine
  -- , intersectionLine'
  -- , intersectionLineWithDefiners
  -- , EdgeDefiners(..)
  ) where

import Control.Lens
import Control.Subcategory.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Monoid (First(..))
import HGeometry.Box
import HGeometry.Cyclic
import HGeometry.Direction
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Plane.LowerEnvelope.Connected.Region
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Polygon.Convex
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Polygon.Simple.Class
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.NonEmpty.Util ()
import GHC.Generics (Generic)
import Control.DeepSeq

--------------------------------------------------------------------------------
-- * The Minimization Diagram, i.e. the type that we use to represent our
-- lower envelope

-- | A minimization daigram just maps every plane on the lower envelope to the region
-- above which it is minimal. Every plane has at most one such a region.
newtype MinimizationDiagram r vertex plane = MinimizationDiagram (NEMap plane (Region r vertex))
  deriving stock (Show,Eq,Generic)
  deriving newtype (NFData)

type instance NumType   (MinimizationDiagram r vertex plane) = r
type instance Dimension (MinimizationDiagram r vertex plane) = 2


instance Constrained (MinimizationDiagram r vertex) where
  type Dom (MinimizationDiagram r vertex) plane = ( Ord plane, NumType plane ~ r
                                                  -- , NumType vertex ~ r
                                                  )

instance CFunctor (MinimizationDiagram r vertex) where
  cmap f (MinimizationDiagram m) = MinimizationDiagram $ NEMap.mapKeys f m

-- | Get the underlying Map that relates every plane in the envelope to its projected region
asMap                         :: MinimizationDiagram r vertex plane
                              -> NEMap plane (Region r vertex)
asMap (MinimizationDiagram m) = m


-- | Apply some mapping function to the vertex data of each vertex
mapVertices                           :: (vertex -> vertex')
                                      -> MinimizationDiagram r vertex plane
                                      -> MinimizationDiagram r vertex' plane
mapVertices f (MinimizationDiagram m) = MinimizationDiagram $ fmap (fmap f) m

-- -- | Apply some mapping function to both the vertices and the planes.
-- cbimap :: (Ord plane, Ord plane', NumType plane ~ r, NumType plane' ~ r)
--        => (vertex -> vertex') -> (plane -> plane')
--        -> MinimizationDiagram r vertex plane
--        -> MinimizationDiagram r vertex' plane'
-- cbimap f g (MinimizationDiagram m) = MinimizationDiagram . fmap (fmap f) $ NEMap.mapKeys g m



--------------------------------------------------------------------------------

-- | Computes a convex polygon corresponding to the region.
--
-- pre: the bounding box (strictly) contains all vertices in its interior
toConvexPolygonIn      :: ( Rectangle_ rectangle corner, Point_ corner 2 r
                          , Point_ point 2 r, Ord r, Fractional r
                          )
                       => rectangle -> Region r point
                       -> Either (ConvexPolygonF (Cyclic NonEmpty) point)
                                 (ConvexPolygonF (Cyclic NonEmpty) (OriginalOrExtra point (Point 2 r)))
toConvexPolygonIn rect = \case
    BoundedRegion convex                -> Left convex
    UnboundedRegion (Unbounded u pts v) ->
                         let p        = NonEmpty.head pts
                             q        = NonEmpty.last pts
                             hp       = HalfLine (p^.asPoint) ((-1) *^ u)
                             hq       = HalfLine (q^.asPoint) v
                             extras   = extraPoints hp hq $ Box (rect^.minPoint.asPoint)
                                                                (rect^.maxPoint.asPoint)
                         in Right . uncheckedFromCCWPoints $
                                (Extra <$> extras) <> (Original <$> pts)

-- | computes the extra vertices that we have to insert to make an unbounded region bounded
extraPoints            :: ( Rectangle_ rectangle corner, Point_ corner 2 r
                          , Point_ point 2 r, Fractional r, Ord r
                          , IsIntersectableWith (HalfLine point) (ClosedLineSegment corner)
                          , Intersection (HalfLine point) (ClosedLineSegment corner)
                            ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                                                                     (ClosedLineSegment corner))
                          )
                       => HalfLine point -> HalfLine point -> rectangle
                       -> NonEmpty (Point 2 r)
extraPoints hp hq rect = noDuplicates $ q :| cornersInBetween qSide pSide rect <> [p]
    -- if the intersection point coincides with a corner then the current code includes
    -- the corner. We use the noDuplicates to get rid of those.
  where
    (q,qSide) = intersectionPoint hq
    (p,pSide) = intersectionPoint hp

    intersectionPoint  h = case getFirst $ intersectionPoint' h of
                             Nothing -> error "extraPoints: precondititon failed "
                             Just x  -> x
    intersectionPoint' h = flip ifoldMap (sides rect) $ \side seg ->
      case h `intersect` seg of
        Just (HalfLine_x_LineSegment_Point x) -> First $ Just (x, side)
        _                                     -> First   Nothing

    noDuplicates = fmap NonEmpty.head . NonEmpty.group1


-- | Computes the corners in between the two given sides (in CCW order)
cornersInBetween          :: (Rectangle_ rectangle point, Point_ point 2 r, Num r)
                          => CardinalDirection -> CardinalDirection -> rectangle -> [Point 2 r]
cornersInBetween s e rect = map snd
                          . takeWhile ((/= e) . fst) . dropWhile ((/= s) . fst)
                          $ cycle [(East,tr),(North,tl),(West,bl),(South,br)]
  where
    Corners tl tr br bl = view asPoint <$> corners rect


--------------------------------------------------------------------------------

-- instance HasVertices' (MinimizationDiagram r plane) where
--   -- | invariant: vertexIx == UnboundedVertex <=> vertex = UnboundedVertex
--   type VertexIx (MinimizationDiagram r plane) = MDVertexIx
--   type Vertex (MinimizationDiagram r plane)   = MDVertex r plane
--   vertexAt = \case
--     UnboundedVertexIx -> pure UnboundedVertex
--     BoundedVertexIx i ->

-- instance HasVertices (MinimizationDiagram r plane) (MinimizationDiagram r plane) where
-- instance HasDarts' (MinimizationDiagram r plane) where
--   type DartIx (MinimizationDiagram r plane) = (MDVertexIx, MDVertexIx)
--   type Dart (MinimizationDiagram r plane)   = (MDVertex r plane, MDVertex r plane)
--   dartAt = undefined

-- instance HasDarts (MinimizationDiagram r plane) (MinimizationDiagram r plane) where

-- instance DiGraph_ (MinimizationDiagram r plane) where
--   diGraphFromAdjacencyLists = undefined
--   endPoints = undefined
--   outNeighboursOf = undefined
--   twinDartOf = undefined
