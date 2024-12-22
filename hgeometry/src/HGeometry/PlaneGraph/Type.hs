{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Type type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Type
  ( PlaneGraph(..)
  -- , fromAdjacencyRep
  -- , fromConnectedSegments
  -- , VertexData(VertexData), location

  -- , E(..)
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Coerce
import           Data.Foldable1
import           Data.Foldable1.WithIndex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Vector.NonEmpty as Vector
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.Foldable.Sort (sortBy )
import           HGeometry.LineSegment
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.PlaneGraph.Class
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Vector
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph ( PlanarGraph, World(..)
                                     , DartId, VertexId, FaceId
                                     )
import           Hiraffe.PlanarGraph.Class
import qualified Hiraffe.PlanarGraph as PG
import qualified Hiraffe.PlanarGraph.Dart as Dart


-- import           Data.YAML

--------------------------------------------------------------------------------
-- * The PlaneGraph type

-- | An Embedded, *connected*, planar graph
newtype PlaneGraph s v e f = PlaneGraph (PlanarGraph Primal s v e f)
      -- deriving stock (Show,Eq,Generic)
      -- deriving newtype (ToYAML,FromYAML)

type instance NumType   (PlaneGraph s v e f) = NumType v
type instance Dimension (PlaneGraph s v e f) = 2

-- | Iso to access the graph
_PlanarGraph :: Iso (PlaneGraph s v e f)         (PlaneGraph s v' e' f')
                    (PlanarGraph Primal s v e f) (PlanarGraph Primal s v' e' f')
_PlanarGraph = coerced
{-# INLINE _PlanarGraph #-}

----------------------------------------

instance HasVertices' (PlaneGraph s v e f) where
  type Vertex   (PlaneGraph s v e f) = v
  type VertexIx (PlaneGraph s v e f) = VertexId s
  vertexAt i = _PlanarGraph.vertexAt i

instance HasVertices (PlaneGraph s v e f) (PlaneGraph s v' e f) where
  vertices = _PlanarGraph.vertices

----------------------------------------

instance HasDarts' (PlaneGraph s v e f) where
  type Dart   (PlaneGraph s v e f) = e
  type DartIx (PlaneGraph s v e f) = DartId s
  dartAt i = _PlanarGraph.dartAt i

instance HasDarts (PlaneGraph s v e f) (PlaneGraph s v e' f) where
  darts = _PlanarGraph.darts

----------------------------------------

instance HasEdges' (PlaneGraph s v e f) where
  type Edge   (PlaneGraph s v e f) = e
  type EdgeIx (PlaneGraph s v e f) = DartId s
  edgeAt d = _PlanarGraph.edgeAt d

instance HasEdges (PlaneGraph s v e f) (PlaneGraph s v e f) where
  edges = _PlanarGraph.edges

----------------------------------------

instance HasFaces' (PlaneGraph s v e f) where
  type Face   (PlaneGraph s v e f) = f
  type FaceIx (PlaneGraph s v e f) = FaceId s
  faceAt fi = _PlanarGraph.faceAt fi


instance HasFaces (PlaneGraph s v e f) (PlaneGraph s v e f') where
  faces = _PlanarGraph.faces

----------------------------------------
instance DiGraph_ (PlaneGraph s v e f) where
  endPoints (PlaneGraph g) = endPoints g
  twinDartOf d = twinOf d . to Just
  outgoingDartsOf v = _PlanarGraph.outgoingDartsOf v

{-
instance ConstructableDiGraph_ (PlaneGraph s v e f) where
  type DiGraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)

  -- | The vertices are expected to have their adjacencies in CCW order.
  diGraphFromAdjacencyLists = PlaneGraph . diGraphFromAdjacencyLists
  -- TODO: we should probably use some toEmbedding here as well I think
-}

instance BidirGraph_ (PlaneGraph s v e f) where
  twinOf d = to $ const (PG.twin d)
  getPositiveDart (PlaneGraph g) e = getPositiveDart g e

instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => Graph_ (PlaneGraph s v e f) where
  neighboursOf u = _PlanarGraph.neighboursOf u
  incidentEdgesOf u = _PlanarGraph.incidentEdgesOf u

{-
instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => ConstructableGraph_ (PlaneGraph s v e f) where
  type GraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)
  fromAdjacencyLists = fromEmbedding . toEmbedding
-}

{-
instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)

         ) => PlanarGraph_ (PlaneGraph s v e f) where
  type DualGraphOf (PlaneGraph s v e f) = PlanarGraph Dual s f e v

  dualGraph = dualGraph . coerce @_ @(PlanarGraph Primal s v e f)

  leftFaceOf  d = _PlanarGraph.leftFaceOf d
  rightFaceOf d = _PlanarGraph.rightFaceOf d

  nextDartOf d = _PlanarGraph.nextDartOf d
  prevDartOf d = _PlanarGraph.prevDartOf d

  boundaryDartOf f = _PlanarGraph.boundaryDartOf f
  boundaryDarts f = boundaryDarts f . coerce @_ @(PlanarGraph Primal s v e f)


instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => PlaneGraph_ (PlaneGraph s v e f) v where
  fromEmbedding = PlaneGraph . fromAdjacencyLists

instance ( Point_ v 2 r, Point_ v' 2 r'
         ) => HasPoints (PlaneGraph s v e f)
                        (PlaneGraph s v' e f) v v' where
  allPoints = vertices

instance ( Point_ v 2 r
         , Ord r, Num r
         ) => IsBoxable (PlaneGraph s v e f)

instance ( Point_ v 2 r
         , DefaultTransformByConstraints (PlaneGraph s v e f) 2 r
         ) => IsTransformable (PlaneGraph s v e f)



  -- boundingBox = boundingBoxList' . F.toList . fmap (^._2.location) . vertices


--------------------------------------------------------------------------------

-- | Constructs a connected plane graph
--
-- pre: The segments form a single connected component
--      No two segments partially overlap.
--
-- running time: \(O(n\log n)\)
fromConnectedSegments      :: ( Foldable1 f, Ord r, Num r
                              , LineSegment_ lineSegment point
                              , Point_ point 2 r
                              )
                           => f lineSegment
                           -> PlaneGraph s (NonEmpty.NonEmpty point) lineSegment ()
fromConnectedSegments segs = PlaneGraph $
                             (PG.planarGraph theDarts)&PG.vertexData .~ vtxData
  where
    -- to get the darts we simply convert the NEMap (_, NEMap _ (dart, seg)) into
    -- a NonEmpty (NonEmpty (dart, seg))
    theDarts = toNonEmpty . snd  <$> verts
    vtxData  = Vector.fromNonEmpty $ fst <$> verts

    -- Collects all edges per vertex
    verts    = toNonEmpty . ifoldMap1 f $ toNonEmpty segs

    -- Creates two vertices with one edge each ; combines them into a single Map
    f i seg = let u = seg^.start
                  v = seg^.end
                  d = Dart.Dart (Dart.Arc i) Dart.Positive
              in    singleton (u^.asPoint) (vtx (d          ,seg) u v)
                 <> singleton (v^.asPoint) (vtx (Dart.twin d,seg) v u)

    singleton k v = MonoidalNEMap $ NEMap.singleton k v

-- | Helper type to represent the vertex data of a vertex. The NEMap
-- represents the edges ordered cyclically around the vertex
type VtxData v r e = (v, NEMap.NEMap (E r) e)

-- | Creates the vertex data
vtx       :: (Point_ point 2 r, Ord r, Num r)
          => e -> point -> point -> VtxData (NonEmpty.NonEmpty point) r e
vtx e p q = (NonEmpty.singleton p, NEMap.singleton (E $ q .-. p) e)

--------------------------------------------------------------------------------

-- | Given a connected plane graph in adjacency list format; convert it into an actual
-- PlaneGraph.
--
-- \(O(n\log n)\)
fromAdjacencyRep       :: (Point_ vertex 2 r, Ord i, Foldable1 f)
                       => proxy s -> GGraph f i vertex e -> PlaneGraph s vertex e ()
fromAdjacencyRep proxy = PlaneGraph . PG.fromAdjacencyRep proxy


--------------------------------------------------------------------------------

-- | Helper type to sort vectors cyclically around the origine
newtype E r = E (Vector 2 r)
  deriving newtype (Show)

instance (Ord r, Num r) => Eq (E r) where
  a == b = a `compare` b == EQ
instance (Ord r, Num r) => Ord (E r) where
  (E v) `compare` (E u) = ccwCmpAroundWith (Vector2 0 1) (origin :: Point 2 r) (Point v) (Point u)

-}
