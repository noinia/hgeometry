{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Connected.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Type type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Connected.Type
  ( CPlaneGraph(..)
  , _CPlanarGraph
  , fromAdjacencyRep
  , fromConnectedSegments
  -- , VertexData(VertexData), location

  , E(..)
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
import           Hiraffe.PlanarGraph.Class
import           Hiraffe.PlanarGraph.Connected ( CPlanarGraph, World(..)
                                               , DartId, VertexId, FaceId
                                               )
import qualified Hiraffe.PlanarGraph.Connected as PG
import qualified Hiraffe.PlanarGraph.Dart as Dart


--------------------------------------------------------------------------------
-- * The CPlaneGraph type

-- | An Embedded, *connected*, planar graph
newtype CPlaneGraph s v e f =
    CPlaneGraph (CPlanarGraph Primal s v e f)
      deriving stock (Show,Eq,Generic)

type instance NumType   (CPlaneGraph s v e f) = NumType v
type instance Dimension (CPlaneGraph s v e f) = 2

-- | Iso to access the underlying planar graph.
--
_CPlanarGraph :: Iso (CPlaneGraph s v e f)         (CPlaneGraph s v' e' f')
                     (CPlanarGraph Primal s v e f) (CPlanarGraph Primal s v' e' f')
_CPlanarGraph = coerced
{-# INLINE _CPlanarGraph #-}

----------------------------------------

instance HasVertices' (CPlaneGraph s v e f) where
  type Vertex   (CPlaneGraph s v e f) = v
  type VertexIx (CPlaneGraph s v e f) = VertexId s
  vertexAt i = _CPlanarGraph.vertexAt i

instance HasVertices (CPlaneGraph s v e f) (CPlaneGraph s v' e f) where
  vertices = _CPlanarGraph.vertices

----------------------------------------

instance HasDarts' (CPlaneGraph s v e f) where
  type Dart   (CPlaneGraph s v e f) = e
  type DartIx (CPlaneGraph s v e f) = DartId s
  dartAt i = _CPlanarGraph.dartAt i

instance HasDarts (CPlaneGraph s v e f) (CPlaneGraph s v e' f) where
  darts = _CPlanarGraph.darts

----------------------------------------

instance HasEdges' (CPlaneGraph s v e f) where
  type Edge   (CPlaneGraph s v e f) = e
  type EdgeIx (CPlaneGraph s v e f) = DartId s
  edgeAt d = _CPlanarGraph.edgeAt d

instance HasEdges (CPlaneGraph s v e f) (CPlaneGraph s v e f) where
  edges = _CPlanarGraph.edges

----------------------------------------

instance HasFaces' (CPlaneGraph s v e f) where
  type Face   (CPlaneGraph s v e f) = f
  type FaceIx (CPlaneGraph s v e f) = FaceId s
  faceAt fi = _CPlanarGraph.faceAt fi


instance HasFaces (CPlaneGraph s v e f) (CPlaneGraph s v e f') where
  faces = _CPlanarGraph.faces

----------------------------------------
instance DiGraph_ (CPlaneGraph s v e f) where
  endPoints (CPlaneGraph g) = endPoints g
  twinDartOf d = twinOf d . to Just
  outgoingDartsOf v = _CPlanarGraph.outgoingDartsOf v

instance ConstructableDiGraph_ (CPlaneGraph s v e f) where
  type DiGraphFromAdjListExtraConstraints (CPlaneGraph s v e f) h = (f ~ (), Foldable1 h)

  -- | The vertices are expected to have their adjacencies in CCW order.
  diGraphFromAdjacencyLists = CPlaneGraph . diGraphFromAdjacencyLists
  -- TODO: we should probably use some toEmbedding here as well I think


instance BidirGraph_ (CPlaneGraph s v e f) where
  twinOf d = to $ const (PG.twin d)
  getPositiveDart (CPlaneGraph g) e = getPositiveDart g e


-- | Computes the cyclic order of adjacencies around each vertex.
--
-- \(O(n \log n)\)
toEmbedding :: ( Foldable1 g, Functor g, Foldable h, Functor h
               , vi ~ VertexIx (CPlaneGraph s v e f)
               , v ~ Vertex (CPlaneGraph s v e f)
               , e ~ Edge (CPlaneGraph s v e f)
               , GraphFromAdjListExtraConstraints (CPlaneGraph s v e f) h
               , Point_ v 2 r, Ord r, Num r
               ) => g (vi, v, h (vi, e)) -> g (vi, v, Vector.NonEmptyVector (vi, e))
toEmbedding vs = fmap sortAround' vs
  where
    vertexLocs             = foldMap (\(vi,v,_) -> Map.singleton vi v) vs
    sortAround' (vi,v,adjs) = (vi,v, Vector.unsafeFromVector $ sortBy (ccwCmpAround' v) adjs)
    ccwCmpAround' v (ui,_) (wi,_) = ccwCmpAround v (vertexLocs Map.! ui) (vertexLocs Map.! wi)



instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => Graph_ (CPlaneGraph s v e f) where
  neighboursOf u = _CPlanarGraph.neighboursOf u
  neighboursOfByEdge u = _CPlanarGraph.neighboursOfByEdge u
  incidentEdgesOf u = _CPlanarGraph.incidentEdgesOf u

instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => ConstructableGraph_ (CPlaneGraph s v e f) where
  type GraphFromAdjListExtraConstraints (CPlaneGraph s v e f) h = (f ~ (), Foldable1 h)

  fromAdjacencyLists = fromEmbedding . toEmbedding


instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => PlanarGraph_ (CPlaneGraph s v e f) where
  type DualGraphOf (CPlaneGraph s v e f) = CPlanarGraph Dual s f e v

  _DualFaceIx _ = coerced
  _DualVertexIx _ = coerced

  dualGraph = dualGraph . coerce @_ @(CPlanarGraph Primal s v e f)

  leftFaceOf  d = _CPlanarGraph.leftFaceOf d
  rightFaceOf d = _CPlanarGraph.rightFaceOf d

  nextDartOf d = _CPlanarGraph.nextDartOf d
  prevDartOf d = _CPlanarGraph.prevDartOf d

  boundaryDartOf f = _CPlanarGraph.boundaryDartOf f
  boundaryDarts f = boundaryDarts f . coerce @_ @(CPlanarGraph Primal s v e f)


instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => PlaneGraph_ (CPlaneGraph s v e f) v where
  fromEmbedding = CPlaneGraph . fromAdjacencyLists

instance ( Point_ v 2 r, Point_ v' 2 r'
         ) => HasPoints (CPlaneGraph s v e f)
                        (CPlaneGraph s v' e f) v v' where
  allPoints = vertices

instance ( Point_ v 2 r
         , Ord r, Num r
         ) => IsBoxable (CPlaneGraph s v e f)

instance ( Point_ v 2 r
         , DefaultTransformByConstraints (CPlaneGraph s v e f) 2 r
         ) => IsTransformable (CPlaneGraph s v e f)



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
                           -> CPlaneGraph s (NonEmpty.NonEmpty point) lineSegment ()
fromConnectedSegments segs = CPlaneGraph $
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
-- CPlaneGraph.
--
-- \(O(n\log n)\)
fromAdjacencyRep       :: (Point_ vertex 2 r, Ord i, Foldable1 f)
                       => proxy s -> GGraph f i vertex e -> CPlaneGraph s vertex e ()
fromAdjacencyRep proxy = CPlaneGraph . PG.fromAdjacencyRep proxy


--------------------------------------------------------------------------------

-- | Helper type to sort vectors cyclically around the origine
newtype E r = E (Vector 2 r)
  deriving newtype (Show)

instance (Ord r, Num r) => Eq (E r) where
  a == b = a `compare` b == EQ
instance (Ord r, Num r) => Ord (E r) where
  (E v) `compare` (E u) = ccwCmpAroundWith (Vector2 0 1) (origin :: Point 2 r) (Point v) (Point u)
