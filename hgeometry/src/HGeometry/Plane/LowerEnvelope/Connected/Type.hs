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
  , Region(..)
  , toConvexPolygonIn

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

import           Control.Lens
import           Control.Subcategory.Functor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Monoid (First(..))
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Direction
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------
-- * The Minimization Diagram, i.e. the type that we use to represent our
-- lower envelope

-- | A minimization daigram just maps every plane on the lower envelope to the region
-- above which it is minimal. Every plane has at most one such a region.
newtype MinimizationDiagram r plane = MinimizationDiagram (NEMap plane (Region r (Point 2 r)))
  deriving stock (Show,Eq)

type instance NumType   (MinimizationDiagram r plane) = r
type instance Dimension (MinimizationDiagram r plane) = 2

instance Constrained (MinimizationDiagram r) where
  type Dom (MinimizationDiagram r) plane = (Ord plane, NumType plane ~ r)

instance CFunctor (MinimizationDiagram r) where
  cmap f (MinimizationDiagram m) = MinimizationDiagram $ NEMap.mapKeys f m



-- | Get the underlying Map that relates every plane in the envelope to its projected region
asMap                         :: MinimizationDiagram r plane
                              -> NEMap plane (Region r (Point 2 r))
asMap (MinimizationDiagram m) = m



-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data Region r point = Bounded   (Cyclic NonEmpty point)
                    | Unbounded (Vector 2 r)
                                -- ^ vector indicating the direction of the unbounded edge
                                -- incident to the first vertex. Note that this vector
                                -- thus points INTO vertex v.
                                (NonEmpty point)
                                -- ^ the vertices in CCW order,
                                (Vector 2 r)
                                -- ^ the vector indicating the direction of the unbounded
                                -- edge incident to the last vertex. The vector points
                                -- away from the vertex (i.e. towards +infty).
                      deriving stock (Show,Eq,Functor,Foldable,Traversable)

type instance NumType   (Region r point) = r
type instance Dimension (Region r point) = Dimension point


-- | Helper type for distinguishing original vertices from extra ones.
data OriginalOrExtra orig extra = Original orig
                                | Extra    extra
                                deriving (Show,Eq,Functor)

type instance NumType   (OriginalOrExtra orig extra) = NumType orig
type instance Dimension (OriginalOrExtra orig extra) = Dimension orig

instance ( HasVector orig orig, HasVector extra extra
         , HasVector orig orig', HasVector extra extra'
         , Dimension extra ~ Dimension orig, NumType extra ~ NumType orig
         , Dimension extra' ~ Dimension orig', NumType extra' ~ NumType orig'
         ) => HasVector (OriginalOrExtra orig extra) (OriginalOrExtra orig' extra') where
  vector = lens g (flip s)
    where
      g = \case
        Original o -> o^.vector
        Extra    e -> e^.vector
      s v = \case
        Original o -> Original (o&vector .~ v)
        Extra    e -> Extra    (e&vector .~ v)


instance ( HasCoordinates orig orig', HasCoordinates extra extra'
         , HasVector orig orig, HasVector extra extra
         , Dimension extra ~ Dimension orig, NumType extra ~ NumType orig
         , Dimension extra' ~ Dimension orig', NumType extra' ~ NumType orig'
         ) => HasCoordinates (OriginalOrExtra orig extra) (OriginalOrExtra orig' extra')

instance (Affine_ orig d r, Affine_ extra d r) => Affine_ (OriginalOrExtra orig extra) d r

instance (Point_ orig d r, Point_ extra d r) => Point_ (OriginalOrExtra orig extra) d r



-- | Computes a convex polygon
--
-- pre: the bounding box (strictly) contains all vertices in its interior
toConvexPolygonIn :: (Rectangle_ rectangle corner, Point_ corner 2 r
                     , Point_ point 2 r, Ord r, Fractional r
                     )
                  => rectangle -> Region r point
                  -> Either (ConvexPolygonF NonEmpty point)
                            (ConvexPolygonF NonEmpty (OriginalOrExtra point (Point 2 r)))
toConvexPolygonIn rect = \case
    Bounded vertices  -> Left $ uncheckedFromCCWPoints vertices
    Unbounded u pts v -> let p        = NonEmpty.head pts
                             q        = NonEmpty.last pts
                             hp       = HalfLine (p^.asPoint) ((-1) *^ u)
                             hq       = HalfLine (q^.asPoint) v
                             extras   = extraPoints hp hq $ Box (rect^.minPoint.asPoint)
                                                                (rect^.maxPoint.asPoint)
                         in Right . uncheckedFromCCWPoints $
                                (Extra <$> extras) <> (Original <$> pts)

-- | computes the extra vertices that we have to insert to make an unbounded region bounded
extraPoints            :: ( Rectangle_ rectangle corner, Point_ corner 2 r
                          , Point_ point 2 r
                          , Fractional r, Ord r
                          , IsIntersectableWith (HalfLine point) (ClosedLineSegment corner)
                          , Intersection (HalfLine point) (ClosedLineSegment corner)
                            ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r) (ClosedLineSegment corner))
                          )
                       => HalfLine point -> HalfLine point -> rectangle
                       -> NonEmpty (Point 2 r)
extraPoints hp hq rect = q :| cornersInBetween qSide pSide rect <> [p]
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

-- | Computes the corners in between the two given sides (in CCW order)
cornersInBetween          :: (Rectangle_ rectangle point, Point_ point 2 r, Num r)
                          => CardinalDirection -> CardinalDirection -> rectangle -> [Point 2 r]
cornersInBetween s e rect = map snd
                          . takeWhile ((/= e) . fst) . dropWhile ((/= s) . fst)
                          $ cycle [(East,tr),(North,tl),(West,bl),(South,br)]
  where
    Corners tl tr br bl = view asPoint <$> corners rect


--------------------------------------------------------------------------------

-- data MDVertexIx = UnboundedVertexIx
--                 | BoundedVertexIx {-# UNPACK #-}!Int
--                 deriving (Show,Read,Eq,Ord)

-- data MDVertex r plane = UnboundedVertex
--                       | BoundedVertex { _location :: Point 2 r
--                                       , _definers :: Vector 3 plane
--                                       }
--                       deriving (Show,Eq,Ord)


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


-- -- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- -- represented by its point, it stores a list of its outgoing edges, and some data.
-- toGraph :: (Plane_ plane r, Num r, Ord r)
--         => MinimizationDiagram r plane -> PlaneGraph (Point 2 r) (First r) (E r)
-- toGraph = mapWithKeyMerge toTriangulatedGr . asMap
