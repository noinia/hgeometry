{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometryPlaneGraph.Type
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
  , fromAdjacencyRep
  -- , VertexData(VertexData), location
  ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Coerce
import           Data.Foldable1
import qualified Data.Map as Map
import qualified Data.Vector.NonEmpty as Vector
import           Data.YAML
import           GHC.Generics (Generic)
import           HGeometry.Box
import           HGeometry.Foldable.Sort (sortBy )
import           HGeometry.PlaneGraph.Class
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.Graph.Class
import           Hiraffe.PlanarGraph ( PlanarGraph, World(..)
                                     , DartId, VertexId, FaceId
                                     )
import qualified Hiraffe.PlanarGraph as PG
import           Hiraffe.PlanarGraph.Class

--------------------------------------------------------------------------------
-- * The PlaneGraph type

-- | An Embedded, *connected*, planar graph
newtype PlaneGraph s v e f =
    PlaneGraph (PlanarGraph s Primal v e f)
      deriving stock (Show,Eq,Generic)
      deriving newtype (ToYAML,FromYAML)

type instance NumType   (PlaneGraph s v e f) = NumType v
type instance Dimension (PlaneGraph s v e f) = 2

-- | Iso to access the graph
_PlanarGraph :: Iso (PlaneGraph s v e f)         (PlaneGraph s v' e' f')
                    (PlanarGraph s Primal v e f) (PlanarGraph s Primal v' e' f')
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
  type DiGraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)

  -- | The vertices are expected to have their adjacencies in CCW order.
  diGraphFromAdjacencyLists = PlaneGraph . diGraphFromAdjacencyLists
  -- TODO: we should probably use some toEmbedding here as well I think

  endPoints (PlaneGraph g) = endPoints g
  twinDartOf d = twinOf d . to Just
  outgoingDartsOf v = _PlanarGraph.outgoingDartsOf v

instance BidirGraph_ (PlaneGraph s v e f) where
  twinOf d = to $ const (PG.twin d)
  getPositiveDart (PlaneGraph g) e = getPositiveDart g e


-- | Computes the cyclic order of adjacencies around each vertex.
--
-- \(O(n \log n)\)
toEmbedding :: ( Foldable1 g, Functor g, Foldable h, Functor h
               , vi ~ VertexIx (PlaneGraph s v e f)
               , v ~ Vertex (PlaneGraph s v e f)
               , e ~ Edge (PlaneGraph s v e f)
               , GraphFromAdjListExtraConstraints (PlaneGraph s v e f) h
               , Point_ v 2 r, Ord r, Num r
               ) => g (vi, v, h (vi, e)) -> g (vi, v, Vector.NonEmptyVector (vi, e))
toEmbedding vs = fmap sortAround' vs
  where
    vertexLocs             = foldMap (\(vi,v,_) -> Map.singleton vi v) vs
    sortAround' (vi,v,adjs) = (vi,v, Vector.unsafeFromVector $ sortBy (ccwCmpAround' v) adjs)
    ccwCmpAround' v (ui,_) (wi,_) = ccwCmpAround v (vertexLocs Map.! ui) (vertexLocs Map.! wi)



instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => Graph_ (PlaneGraph s v e f) where
  type GraphFromAdjListExtraConstraints (PlaneGraph s v e f) h = (f ~ (), Foldable1 h)

  fromAdjacencyLists = fromEmbedding . toEmbedding

  neighboursOf u = _PlanarGraph.neighboursOf u
  incidentEdgesOf u = _PlanarGraph.incidentEdgesOf u

instance ( Point_ v 2 (NumType v)
         , Ord (NumType v), Num (NumType v)
         ) => PlanarGraph_ (PlaneGraph s v e f) where
  type DualGraphOf (PlaneGraph s v e f) = PlanarGraph s Dual f e v

  dualGraph = dualGraph . coerce @_ @(PlanarGraph s Primal v e f)

  leftFaceOf  d = _PlanarGraph.leftFaceOf d
  rightFaceOf d = _PlanarGraph.rightFaceOf d

  nextDartOf d = _PlanarGraph.nextDartOf d
  prevDartOf d = _PlanarGraph.prevDartOf d

  boundaryDartOf f = _PlanarGraph.boundaryDartOf f
  boundaryDarts f = boundaryDarts f . coerce @_ @(PlanarGraph s Primal v e f)


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

{-
-- | Constructs a connected plane graph
--
-- pre: The segments form a single connected component
--
-- running time: \(O(n\log n)\)
fromConnectedSegments      :: ( Foldable1 f, Ord r, Num r
                              , LineSegment_ lineSegment point
                              , Point_ point 2 r
                              )
                           => f lineSegment
                           -> PlaneGraph s (NonEmpty.NonEmpty point) lineSegment () r
fromConnectedSegments segs = PlaneGraph $ planarGraph dts & PG.vertexData .~ vxData
  where
    -- compute the points, with for every point a list of its darts around it




    pts :: NEMap point [(point, (dart, lineSegment))]
    pts = ifoldMap1 (\i seg -> NEMap.sigleton (seg^.start)

                    ) (toNonEmpty segs)

      -- foldMap1 (\

      --              )

      --   $ NonEmpty.zipWith mkArc (0 :| [1..])








    pts         = Map.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ segs



    f (s :+ e)  = [ ( s^.start.core
                    , SP (sing $ s^.start.extra) [(s^.end.core)   :+ h Positive e])
                  , ( s^.end.core
                    , SP (sing $ s^.end.extra)   [(s^.start.core) :+ h Negative e])
                  ]

    mkArc i seg = seg :+

    (s :+ e) = s :+ (Arc i :+ e)



    h d (a :+ e) = (Dart a d, e)

    sing = NonEmpty.singleton

    vts    ::
    vts    = map (\(p,sp) -> (p,map (^.extra) . sortAround' (ext p) <$> sp))
           . Map.assocs $ pts

    -- vertex Data
    vxData = V.fromList . map (\(p,sp) -> VertexData p (sp^._1)) $ vts
    -- The darts
    dts    = fmap (^._2._2) vts

-}


-- | Given a connected plane graph in adjacency list format; convert it into an actual
-- PlaneGraph.
--
-- \(O(n\log n)\)
fromAdjacencyRep       :: (Point_ vertex 2 r, Ord i, Foldable1 f)
                       => proxy s -> GGraph f i vertex e -> PlaneGraph s vertex e ()
fromAdjacencyRep proxy = PlaneGraph . PG.fromAdjacencyRep proxy


-- toPlaneGraph proxy (Graph m) = gr&vertexData .~  (\(VertexData x _ _) -> x <$> fromFoldable1 m)
--   where
--     gr = PlaneGraph $ planarGraph theDarts

--     vtxData =

--     --  a non-empty list of vertices, with for each vertex the darts in order around the vertex
--     theDarts  = evalState (sequence' theDarts) (0 :+ Map.empty)

--     theDarts' = toNonEmpty $ imap toIncidentDarts m
--     -- turn the outgoing edges of u into darts
--     toIncidentDarts u (VertexData _ neighMap neighOrder) =
--       (\v -> (toDart u v, neighMap Map.! u)) <$> toNonEmpty neighOrder
--     -- create the dart corresponding to vertices u and v

--     toDart u v | u <= v    =  flip Dart Positive <$> arc u v
--                | otherwise =  flip Dart Negative <$> arc v u

--     arc u v = gets (arcOf (u,v)) >>= \case
--                 Just a  -> pure a
--                 Nothing -> do a <- nextArc
--                               modify $ insertArc (u,v) a
--                               pure a

--     arcOf x       = Map.lookup x . view extra
--     insertArc k v = over extra $ Map.insert k v

--     nextArc = do i <- gets (view core)
--                  modify $ over core (+1)
--                  pure $ Arc i
