{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.PlanarSubdivision( VertexId', FaceId'
                                      , VertexData(VertexData), vData, location

                                      , EdgeType(..)
                                      , EdgeData(EdgeData), edgeType, eData

                                      , FaceData(FaceData), holes, fData

                                      , PlanarSubdivision(PlanarSubdivision), graph
                                      , PolygonFaceData(..)
                                      , fromPolygon, fromConnectedSegments

                                      , numVertices, numEdges, numFaces, numDarts

                                      , vertices', vertices
                                      , edges', edges
                                      , faces', faces
                                      , darts'

                                      , headOf, tailOf, twin, endPoints, edgeTypeOf

                                      , incidentEdges, incomingEdges, outgoingEdges
                                      , neighboursOf

                                      , leftFace, rightFace
                                      , boundary, boundaryVertices, holesOf
                                      , outerFaceId

                                      , locationOf, vDataOf

                                      , eDataOf, endPointsOf, endPointData
                                      , fDataOf

                                      , edgeSegments
                                      , rawFacePolygon, rawFaceBoundary
                                      , rawFacePolygons

                                      , VertexId(..), FaceId(..), Dart, World(..)
                                      ) where

import           Control.Lens hiding (holes, holesOf)
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.Interval
import           Data.Geometry.Line (cmpSlope, supportingLine)
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.PlanarGraph( PlanarGraph, planarGraph
                                 , Dart(..), VertexId(..), FaceId(..), Arc(..)
                                 , Direction(..), twin
                                 , World(..))
import qualified Data.PlanarGraph as PG
import           Data.Semigroup
import           Data.Util
import qualified Data.Vector as V

import Debug.Trace

--------------------------------------------------------------------------------

type VertexId' s = VertexId s Primal_

type FaceId' s = FaceId s Primal_

-- | Note that the functor instance is in v
data VertexData r v = VertexData { _location :: !(Point 2 r)
                                 , _vData    :: !v
                                 } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''VertexData

vtxDataToExt                  :: VertexData r v -> Point 2 r :+ v
vtxDataToExt (VertexData p v) = p :+ v

instance Bifunctor VertexData where
  bimap f g (VertexData p v) = VertexData (fmap f p) (g v)


-- | Planar-subdivsions are internally represented as a *connected* planar
-- graph. We distuinish two types of edges in this graph representation:
-- Visible edges, which also appear in the original planar subdivision, and
-- Invisible edges, which are essentially dummy edges making sure that the
-- entire graph is connected.
data EdgeType = Visible | Invisible deriving (Show,Read,Eq,Ord)

data EdgeData e = EdgeData { _edgeType :: !EdgeType
                           , _eData    :: !e
                           } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''EdgeData


-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _holes :: [h]
                             , _fData :: !f
                             } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''FaceData


newtype PlanarSubdivision s v e f r = PlanarSubdivision { _graph ::
    PlanarGraph s Primal_ (VertexData r v) (EdgeData e) (FaceData (Dart s) f) }
      deriving (Show,Eq)
makeLenses ''PlanarSubdivision

instance Functor (PlanarSubdivision s v e f) where
  fmap f s = s&graph.PG.vertexData.traverse.location %~ fmap f


--------------------------------------------------------------------------------
-- * Constructing a planar subdivision

data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)

-- | Construct a planar subdivision from a polygon
--
-- running time: \(O(n)\).
fromPolygon                            :: proxy s
                                       -> SimplePolygon p r
                                       -> f -- ^ data inside
                                       -> f -- ^ data outside the polygon
                                       -> PlanarSubdivision s p () f r
fromPolygon p (SimplePolygon vs) iD oD = PlanarSubdivision g'
  where
    g      = fromVertices p vs
    fData' = V.fromList [FaceData [] iD, FaceData [] oD]

    g'     = g & PG.faceData             .~ fData'
               & PG.dartData.traverse._2 .~ EdgeData Visible ()
-- The following does not really work anymore
-- frompolygon p (MultiPolygon vs hs) iD oD = PlanarSubdivision g'
--   where
--     g      = fromVertices p vs
--     hs'    = map (\h -> fromPolygon p h oD iD) hs
--            -- note that oD and iD are exchanged
--     fData' = V.fromList [FaceData iD hs', FaceData oD []]


fromVertices      :: proxy s
                  -> C.CSeq (Point 2 r :+ p)
                  -> PlanarGraph s Primal_ (VertexData r p) () ()
fromVertices _ vs = g&PG.vertexData .~ vData'
  where
    n = length vs
    g = planarGraph [ [ (Dart (Arc i)               Positive, ())
                      , (Dart (Arc $ (i+1) `mod` n) Negative, ())
                      ]
                    | i <- [0..(n-1)]]
    vData' = V.fromList . map (\(p :+ e) -> VertexData p e) . F.toList $ vs


-- | Constructs a connected planar subdivision.
--
-- pre: the segments form a single connected component
-- running time: \(O(n\log n)\)
fromConnectedSegments       :: (Foldable f, Ord r, Num r)
                            => proxy s
                            -> f (LineSegment 2 p r :+ EdgeData e)
                            -> PlanarSubdivision s (NonEmpty.NonEmpty p) e () r
fromConnectedSegments px ss = PlanarSubdivision $
    fromConnectedSegments' px ss & PG.faceData.traverse %~ FaceData []

-- | Constructs a planar graph
--
-- pre: The segments form a single connected component
--
-- running time: \(O(n\log n)\)
fromConnectedSegments'      :: (Foldable f, Ord r, Num r)
                            => proxy s
                            -> f (LineSegment 2 p r :+ e)
                            -> PlanarGraph s Primal_
                                  (VertexData r (NonEmpty.NonEmpty p)) e ()
fromConnectedSegments' _ ss = planarGraph dts & PG.vertexData .~ vxData
  where
    pts         = M.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ ss
    f (s :+ e)  = [ ( s^.start.core
                    , SP (sing $ s^.start.extra) [(s^.end.core)   :+ h Positive e])
                  , ( s^.end.core
                    , SP (sing $ s^.end.extra)   [(s^.start.core) :+ h Negative e])
                  ]
    g i (s :+ e) = s :+ (Arc i :+ e)
    h d (a :+ e) = (Dart a d, e)

    sing x = x NonEmpty.:| []

    vts    = map (\(p,sp) -> (p,map (^.extra) . sortArround (ext p) <$> sp))
           . M.assocs $ pts
    -- vertex Data
    vxData = V.fromList . map (\(p,sp) -> VertexData p (sp^._1)) $ vts
    -- The darts
    dts    = map (^._2._2) vts

--------------------------------------------------------------------------------
-- * Basic Graph information

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 4
numVertices :: PlanarSubdivision s v e f r  -> Int
numVertices = PG.numVertices . _graph

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: PlanarSubdivision s v e f r  -> Int
numDarts = PG.numDarts . _graph

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: PlanarSubdivision s v e f r  -> Int
numEdges = PG.numEdges . _graph

-- | Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces :: PlanarSubdivision s v e f r  -> Int
numFaces = PG.numFaces . _graph

-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlanarSubdivision s v e f r  -> V.Vector (VertexId' s)
vertices' = PG.vertices' . _graph

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices   :: PlanarSubdivision s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices = PG.vertices . _graph

-- | Enumerate all darts
darts' :: PlanarSubdivision s v e f r  -> V.Vector (Dart s)
darts' = PG.darts' . _graph

-- | Enumerate all edges. We report only the Positive darts
edges' :: PlanarSubdivision s v e f r  -> V.Vector (Dart s)
edges' = PG.edges' . _graph

-- | Enumerate all edges with their edge data. We report only the Positive
-- darts.
--
-- >>> mapM_ print $ edges myGraph
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
edges :: PlanarSubdivision s v e f r  -> V.Vector (Dart s, EdgeData e)
edges = PG.edges . _graph

-- | Enumerate all faces in the planar subdivision
faces' :: PlanarSubdivision s v e f r  -> V.Vector (FaceId' s)
faces' = PG.faces' . _graph

-- | All faces with their face data.
faces :: PlanarSubdivision s v e f r  -> V.Vector (FaceId' s, FaceData (Dart s) f)
faces = PG.faces . _graph

-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- running time: \(O(1)\)
tailOf   :: Dart s -> PlanarSubdivision s v e f r  -> VertexId' s
tailOf d = PG.tailOf d . _graph

-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
headOf   :: Dart s -> PlanarSubdivision s v e f r  -> VertexId' s
headOf d = PG.headOf d . _graph

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
endPoints   :: Dart s -> PlanarSubdivision s v e f r
            -> (VertexId' s, VertexId' s)
endPoints d = PG.endPoints d . _graph

edgeTypeOf   :: Dart s -> Lens' (PlanarSubdivision s v e f r ) EdgeType
edgeTypeOf d = graph.PG.eDataOf d.edgeType


-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k\) is the output size
incidentEdges   :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (Dart s)
incidentEdges v = PG.incidentEdges v . _graph

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges   :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (Dart s)
incomingEdges v = PG.incomingEdges v . _graph

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges   :: VertexId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
outgoingEdges v = PG.outgoingEdges v . _graph

-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf   :: VertexId' s -> PlanarSubdivision s v e f r
               -> V.Vector (VertexId' s)
neighboursOf v = PG.neighboursOf v . _graph

-- | The face to the left of the dart
--
-- >>> leftFace (dart 1 "+1") myGraph
-- FaceId 1
-- >>> leftFace (dart 1 "-1") myGraph
-- FaceId 2
-- >>> leftFace (dart 2 "+1") myGraph
-- FaceId 2
-- >>> leftFace (dart 0 "+1") myGraph
-- FaceId 0
--
-- running time: \(O(1)\).
leftFace   :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
leftFace d = PG.leftFace d . _graph

-- | The face to the right of the dart
--
-- >>> rightFace (dart 1 "+1") myGraph
-- FaceId 2
-- >>> rightFace (dart 1 "-1") myGraph
-- FaceId 1
-- >>> rightFace (dart 2 "+1") myGraph
-- FaceId 1
-- >>> rightFace (dart 0 "+1") myGraph
-- FaceId 1
--
-- running time: \(O(1)\).
rightFace   :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
rightFace d = PG.rightFace d . _graph


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundary   :: FaceId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
boundary f = PG.boundary f . _graph


-- | The vertices bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices   :: FaceId' s -> PlanarSubdivision s v e f r
                   -> V.Vector (VertexId' s)
boundaryVertices f = PG.boundaryVertices f . _graph


-- | Lists the holes in this face, given as a list of darts to arbitrary darts
-- on those faces.
--
-- running time: \(O(k)\), where \(k\) is the number of darts returned.
holesOf   :: FaceId' s -> PlanarSubdivision s v e f r -> [Dart s]
holesOf f = view (graph.PG.fDataOf f.holes)

--------------------------------------------------------------------------------
-- * Access data


locationOf   :: VertexId' s -> Lens' (PlanarSubdivision s v e f r ) (Point 2 r)
locationOf v = graph.PG.vDataOf v.location

-- | Get the vertex data associated with a node. Note that updating this data may be
-- expensive!!
--
-- running time: \(O(1)\)
vDataOf   :: VertexId' s -> Lens' (PlanarSubdivision s v e f r ) v
vDataOf v = graph.PG.vDataOf v.vData

-- | Edge data of a given dart
--
-- running time: \(O(1)\)
eDataOf   :: Dart s -> Lens' (PlanarSubdivision s v e f r ) e
eDataOf d = graph.PG.eDataOf d.eData

-- | Data of a face of a given face
--
-- running time: \(O(1)\)
fDataOf   :: FaceId' s -> Lens' (PlanarSubdivision s v e f r ) f
fDataOf f = graph.PG.fDataOf f.fData


-- class HasData t v e f r where
--   type DataOf t v e f r
--   dataOf :: t -> Lens' (PlanarSubdivision s v e f r) (DataOf t v e f r)


endPointsOf   :: Dart s -> Getter (PlanarSubdivision s v e f r )
                                  (VertexData r v, VertexData r v)
endPointsOf d = graph.PG.endPointDataOf d


-- | Data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData   :: Dart s -> PlanarSubdivision s v e f r
               ->  (VertexData r v, VertexData r v)
endPointData d = PG.endPointData d . _graph

--------------------------------------------------------------------------------

-- | gets the id of the outer face
--
-- running time: \(O(n)\)
outerFaceId    :: (Ord r, Fractional r) => PlanarSubdivision s v e f r -> FaceId' s
outerFaceId ps = leftFace d ps
  where
    (v,_)  = V.minimumBy (comparing (^._2.location.xCoord)) . vertices $ ps
    d :+ _ = V.maximumBy (cmpSlope `on` (^.extra))
           .  fmap (\d' -> d' :+ (edgeSegment d' ps)^.core.to supportingLine)
           $ incidentEdges v ps
    -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
    -- basically: find the leftmost vertex, find the incident edge with the largest slope
    -- and take the face left of that edge. This is the outerface.
    -- note that this requires that the edges are straight line segments
    --

--------------------------------------------------------------------------------

-- | Reports all visible segments as line segments
edgeSegments    :: PlanarSubdivision s v e f r -> [(Dart s, LineSegment 2 v r :+ e)]
edgeSegments ps = mapMaybe withSegment . F.toList . edges $ ps
  where
    withSegment (d,EdgeData et e) = let (p,q) = bimap vtxDataToExt vtxDataToExt
                                              $ ps^.endPointsOf d
                                        seg   = ClosedLineSegment p q
                                    in case et of
                                         Visible   -> Just (d, seg :+ e)
                                         Invisible -> Nothing


-- | Given a dart and the subdivision constructs the line segment representing it
--
-- \(O(1)\)
edgeSegment      :: Dart s -> PlanarSubdivision s v e f r -> LineSegment 2 v r :+ e
edgeSegment d ps = seg :+ ps^.eDataOf d
  where
    seg = let (p,q) = bimap vtxDataToExt vtxDataToExt $ ps^.endPointsOf d
          in ClosedLineSegment p q

rawFaceBoundary      :: FaceId' s -> PlanarSubdivision s v e f r
                    -> SimplePolygon v r :+ f
rawFaceBoundary i ps = pg :+ (ps^.fDataOf i)
  where
    pg = fromPoints . F.toList . fmap (\j -> ps^.graph.PG.vDataOf j.to vtxDataToExt)
       . boundaryVertices i $ ps

rawFacePolygon :: FaceId' s -> PlanarSubdivision s v e f r
                    -> SomePolygon v r :+ f
rawFacePolygon i ps = case holesOf i ps of
                        [] -> Left  res                               :+ x
                        hs -> Right (MultiPolygon vs $ map toHole hs) :+ x
  where
    res@(SimplePolygon vs) :+ x = rawFaceBoundary i ps
    toHole d = (rawFaceBoundary (leftFace d ps) ps)^.core



-- | Lists all faces of the planar graph. This ignores invisible edges
rawFacePolygons    :: PlanarSubdivision s v e f r
                   -> V.Vector (FaceId' s, SomePolygon v r :+ f)
rawFacePolygons ps = fmap (\i -> (i,rawFacePolygon i ps)) . faces' $ ps
--   where
--     gr = ps^.graph

--     f (i,FaceData hs x) = case hs of
--                             [] -> Left  $ toPolygon i :+ x
--                             _  -> Right

--     toPolygon d = boundaryVertices gr
