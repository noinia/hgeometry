{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.Core
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.Core( -- $setup
                             PlaneGraph(PlaneGraph), graph
                           , PlanarGraph
                           , VertexData(VertexData), vData, location, vtxDataToExt
                           , fromSimplePolygon, fromConnectedSegments
                           , PG.fromAdjacencyLists

                           , numVertices, numEdges, numFaces, numDarts
                           , dual

                           , vertices', vertices
                           , edges', edges
                           , faces', faces, internalFaces, faces''
                           , darts', darts
                           , traverseVertices, traverseDarts, traverseFaces

                           , headOf, tailOf, twin, endPoints

                           , incidentEdges, incomingEdges, outgoingEdges
                           , neighboursOf
                           , nextIncidentEdge, prevIncidentEdge


                           , leftFace, rightFace
                           , nextEdge, prevEdge
                           , boundary, boundary', boundaryDart, boundaryVertices
                           , outerFaceId, outerFaceDart

                           , vertexDataOf, locationOf, HasDataOf(..)

                           , endPointsOf, endPointData
                           , vertexData, faceData, dartData, rawDartData

                           , edgeSegment, edgeSegments
                           , rawFacePolygon, rawFaceBoundary
                           , rawFacePolygons

                           , VertexId(..), FaceId(..), Dart, World(..), VertexId', FaceId'


                           , withEdgeDistances
                           -- , writePlaneGraph, readPlaneGraph
                           ) where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Aeson
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Line (cmpSlope, supportingLine)
import           Data.Geometry.LineSegment hiding (endPoints)
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Ord (comparing)
import qualified Data.PlanarGraph as PG
import           Data.PlanarGraph( PlanarGraph, planarGraph, dual
                                 , Dart(..), VertexId(..), FaceId(..), Arc(..)
                                 , Direction(..), twin
                                 , World(..)
                                 , FaceId', VertexId'
                                 , HasDataOf(..)
                                 )
import           Data.Util
import qualified Data.Vector as V
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Proxy
-- >>> import Data.PlaneGraph.AdjRep(Gr(Gr),Face(Face),Vtx(Vtx))
-- >>> import Data.PlaneGraph.IO(fromAdjRep)
-- >>> import Data.PlanarGraph.Dart(Dart(..),Arc(..))
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
--     small :: Gr (Vtx Int String Int) (Face String)
--     small = Gr [ Vtx 0 (Point2 0 0) [ (2,"0->2")
--                                     , (1,"0->1")
--                                     , (3,"0->3")
--                                     ] 0
--                , Vtx 1 (Point2 2 2) [ (0,"1->0")
--                                     , (2,"1->2")
--                                     , (3,"1->3")
--                                     ] 1
--                , Vtx 2 (Point2 2 0) [ (0,"2->0")
--                                     , (1,"2->1")
--                                     ] 2
--                , Vtx 3 (Point2 (-1) 4) [ (0,"3->0")
--                                        , (1,"3->1")
--                                        ] 3
--                ]
--                [ Face (2,1) "OuterFace"
--                , Face (0,1) "A"
--                , Face (1,0) "B"
--                ]
--     smallG = fromAdjRep (Proxy :: Proxy ()) small
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlaneGraph/small.png)


--------------------------------------------------------------------------------
-- * Vertex Data

-- | Note that the functor instance is in v
data VertexData r v = VertexData { _location :: !(Point 2 r)
                                 , _vData    :: !v
                                 } deriving (Show,Eq,Ord,Generic
                                            ,Functor,Foldable,Traversable)
makeLenses ''VertexData

vtxDataToExt                  :: VertexData r v -> Point 2 r :+ v
vtxDataToExt (VertexData p v) = p :+ v

instance Bifunctor VertexData where
  bimap f g (VertexData p v) = VertexData (fmap f p) (g v)

instance (FromJSON r, FromJSON v) => FromJSON (VertexData r v) where
  parseJSON = fmap (\(l :+ d) -> VertexData l d) . parseJSON

instance (ToJSON r, ToJSON v) => ToJSON (VertexData r v) where
  toJSON     = toJSON     . vtxDataToExt
  toEncoding = toEncoding . vtxDataToExt

--------------------------------------------------------------------------------
-- * The PlaneGraph type

-- | Embedded, *connected*, planar graph
newtype PlaneGraph s v e f r =
    PlaneGraph { _graph :: PlanarGraph s Primal (VertexData r v) e f }
      deriving (Show,Eq,Generic)
makeLenses ''PlaneGraph

type instance NumType   (PlaneGraph s v e f r) = r
type instance Dimension (PlaneGraph s v e f r) = 2

instance Functor (PlaneGraph s v e f) where
  fmap f pg = pg&graph.PG.vertexData.traverse.location %~ fmap f

instance IsBoxable (PlaneGraph s v e f r) where
  boundingBox = boundingBoxList' . F.toList . fmap (^._2.location) . vertices


--------------------------------------------------------------------------------
-- * Constructing a Plane Graph

-- | Construct a plane graph from a simple polygon. It is assumed that the
-- polygon is given in counterclockwise order.
--
-- the interior of the polygon will have faceId 0
--
-- pre: the input polygon is given in counterclockwise order
-- running time: \(O(n)\).
fromSimplePolygon                            :: proxy s
                                             -> SimplePolygon p r
                                             -> f -- ^ data inside
                                             -> f -- ^ data outside the polygon
                                             -> PlaneGraph s p () f r
fromSimplePolygon p (SimplePolygon vs) iD oD = PlaneGraph g'
  where
    g      = fromVertices p vs
    fData' = V.fromList [iD, oD]
    g'     = g & PG.faceData .~ fData'

-- | Constructs a planar from the given vertices
fromVertices      :: proxy s
                  -> C.CSeq (Point 2 r :+ p)
                  -> PlanarGraph s Primal (VertexData r p) () ()
fromVertices _ vs = g&PG.vertexData .~ vData'
  where
    n = length vs
    g = planarGraph [ [ (Dart (Arc i)               Positive, ())
                      , (Dart (Arc $ (i+1) `mod` n) Negative, ())
                      ]
                    | i <- [0..(n-1)]]
    vData' = V.fromList . map (\(p :+ e) -> VertexData p e) . F.toList $ vs

-- | Constructs a connected plane graph
--
-- pre: The segments form a single connected component
--
-- running time: \(O(n\log n)\)
fromConnectedSegments      :: (Foldable f, Ord r, Num r)
                           => proxy s
                           -> f (LineSegment 2 p r :+ e)
                           -> PlaneGraph s (NonEmpty.NonEmpty p) e () r
fromConnectedSegments _ ss = PlaneGraph $ planarGraph dts & PG.vertexData .~ vxData
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

    vts    = map (\(p,sp) -> (p,map (^.extra) . sortAround (ext p) <$> sp))
           . M.assocs $ pts
    -- vertex Data
    vxData = V.fromList . map (\(p,sp) -> VertexData p (sp^._1)) $ vts
    -- The darts
    dts    = map (^._2._2) vts


--------------------------------------------------------------------------------
-- * Basic Graph information

-- | Get the number of vertices
--
-- >>> numVertices smallG
-- 4
numVertices :: PlaneGraph s v e f r  -> Int
numVertices = PG.numVertices . _graph

-- | Get the number of Darts
--
-- >>> numDarts smallG
-- 10
numDarts :: PlaneGraph s v e f r  -> Int
numDarts = PG.numDarts . _graph

-- | Get the number of Edges
--
-- >>> numEdges smallG
-- 5
numEdges :: PlaneGraph s v e f r  -> Int
numEdges = PG.numEdges . _graph

-- | Get the number of faces
--
-- >>> numFaces smallG
-- 3
numFaces :: PlaneGraph s v e f r  -> Int
numFaces = PG.numFaces . _graph

-- | Enumerate all vertices
--
-- >>> vertices' smallG
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlaneGraph s v e f r  -> V.Vector (VertexId' s)
vertices' = PG.vertices' . _graph

-- | Enumerate all vertices, together with their vertex data
--
-- >>> mapM_ print $ vertices smallG
-- (VertexId 0,VertexData {_location = Point2 [0,0], _vData = 0})
-- (VertexId 1,VertexData {_location = Point2 [2,2], _vData = 1})
-- (VertexId 2,VertexData {_location = Point2 [2,0], _vData = 2})
-- (VertexId 3,VertexData {_location = Point2 [-1,4], _vData = 3})
vertices   :: PlaneGraph s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices = PG.vertices . _graph

-- | Enumerate all darts
darts' :: PlaneGraph s v e f r  -> V.Vector (Dart s)
darts' = PG.darts' . _graph

-- | Get all darts together with their data
darts :: PlaneGraph s v e f r  -> V.Vector (Dart s, e)
darts = PG.darts . _graph

-- | Enumerate all edges. We report only the Positive darts
edges' :: PlaneGraph s v e f r  -> V.Vector (Dart s)
edges' = PG.edges' . _graph

-- | Lens to access the raw dart data, use at your own risk
rawDartData :: Lens (PlaneGraph s v e f r) (PlaneGraph s v e' f r)
                    (V.Vector e) (V.Vector e')
rawDartData = graph.PG.rawDartData

-- | lens to access the Dart Data
dartData :: Lens (PlaneGraph s v e f r) (PlaneGraph s v e' f r)
                 (V.Vector (Dart s, e)) (V.Vector (Dart s, e'))
dartData = graph.PG.dartData

-- | Lens to access face data
faceData :: Lens (PlaneGraph s v e f r) (PlaneGraph s v e f' r)
                 (V.Vector f) (V.Vector f')
faceData = graph.PG.faceData

vertexData :: Lens (PlaneGraph s v e f r) (PlaneGraph s v' e f r)
                   (V.Vector v) (V.Vector v')
vertexData = lens get'' set''
  where
    get'' pg    = let v = pg^.graph.PG.vertexData in (^.vData) <$> v
    set'' pg v' = pg&graph.PG.vertexData %~ V.zipWith f v'
    f x (VertexData l _) = VertexData l x

-- | Enumerate all edges with their edge data. We report only the Positive
-- darts.
--
-- >>> mapM_ print $ edges smallG
-- (Dart (Arc 0) +1,"0->2")
-- (Dart (Arc 1) +1,"0->1")
-- (Dart (Arc 2) +1,"0->3")
-- (Dart (Arc 4) +1,"1->2")
-- (Dart (Arc 3) +1,"1->3")
edges :: PlaneGraph s v e f r  -> V.Vector (Dart s, e)
edges = PG.edges . _graph

-- | Enumerate all faces in the plane graph
faces' :: PlaneGraph s v e f r  -> V.Vector (FaceId' s)
faces' = PG.faces' . _graph

-- | All faces with their face data.
--
-- >>> mapM_ print $ faces smallG
-- (FaceId 0,"OuterFace")
-- (FaceId 1,"A")
-- (FaceId 2,"B")
faces :: PlaneGraph s v e f r  -> V.Vector (FaceId' s, f)
faces = PG.faces . _graph


-- | Reports the outerface and all internal faces separately.
-- running time: \(O(n)\)
faces''   :: (Ord r, Fractional r)
          => PlaneGraph s v e f r -> ((FaceId' s, f), V.Vector (FaceId' s, f))
faces'' g = let i = outerFaceId g
            in ((i,g^.dataOf i), V.filter (\(j,_) -> i /= j) $ faces g)

-- | Reports all internal faces.
-- running time: \(O(n)\)
internalFaces :: (Ord r, Fractional r)
              => PlaneGraph s v e f r -> V.Vector (FaceId' s, f)
internalFaces = snd . faces''

-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- running time: \(O(1)\)
--
-- >>> tailOf (dart 0 "+1") smallG
-- VertexId 0
tailOf   :: Dart s -> PlaneGraph s v e f r  -> VertexId' s
tailOf d = PG.tailOf d . _graph

-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
--
-- >>> headOf (dart 0 "+1") smallG
-- VertexId 2
headOf   :: Dart s -> PlaneGraph s v e f r  -> VertexId' s
headOf d = PG.headOf d . _graph

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
--
-- >>> endPoints (dart 0 "+1") smallG
-- (VertexId 0,VertexId 2)
endPoints   :: Dart s -> PlaneGraph s v e f r
            -> (VertexId' s, VertexId' s)
endPoints d = PG.endPoints d . _graph

-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k\) is the output size
--
-- >>> incidentEdges (VertexId 1) smallG
-- [Dart (Arc 1) -1,Dart (Arc 4) +1,Dart (Arc 3) +1]
incidentEdges   :: VertexId' s -> PlaneGraph s v e f r -> V.Vector (Dart s)
incidentEdges v = PG.incidentEdges v . _graph


-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
--
-- >>> incomingEdges (VertexId 1) smallG
-- [Dart (Arc 1) +1,Dart (Arc 4) -1,Dart (Arc 3) -1]
incomingEdges   :: VertexId' s -> PlaneGraph s v e f r -> V.Vector (Dart s)
incomingEdges v = PG.incomingEdges v . _graph



-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
--
-- >>> outgoingEdges (VertexId 1) smallG
-- [Dart (Arc 1) -1,Dart (Arc 4) +1,Dart (Arc 3) +1]
outgoingEdges   :: VertexId' s -> PlaneGraph s v e f r  -> V.Vector (Dart s)
outgoingEdges v = PG.outgoingEdges v . _graph

-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
--
-- >>> neighboursOf (VertexId 1) smallG
-- [VertexId 0,VertexId 2,VertexId 3]
neighboursOf   :: VertexId' s -> PlaneGraph s v e f r
               -> V.Vector (VertexId' s)
neighboursOf v = PG.neighboursOf v . _graph

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v in clockwise direction.
--
-- running time: \(O(1)\)
--
-- >>> nextIncidentEdge (dart 1 "+1") smallG
-- Dart (Arc 2) +1
nextIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextIncidentEdge d = PG.nextIncidentEdge d . _graph

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v (in clockwise order)
--
-- running time: \(O(1)\)
--
-- >>> prevIncidentEdge (dart 1 "+1") smallG
-- Dart (Arc 0) +1
prevIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevIncidentEdge d = PG.prevIncidentEdge d . _graph


-- | The face to the left of the dart
--
-- running time: \(O(1)\).
--
-- >>> leftFace (dart 1 "+1") smallG
-- FaceId 2
-- >>> leftFace (dart 1 "-1") smallG
-- FaceId 1
-- >>> leftFace (dart 2 "+1") smallG
-- FaceId 0
-- >>> leftFace (dart 2 "-1") smallG
-- FaceId 2
leftFace   :: Dart s -> PlaneGraph s v e f r  -> FaceId' s
leftFace d = PG.leftFace d . _graph

-- | The face to the right of the dart
--
-- running time: \(O(1)\).
--
-- >>> rightFace (dart 1 "+1") smallG
-- FaceId 1
-- >>> rightFace (dart 1 "-1") smallG
-- FaceId 2
-- >>> rightFace (dart 2 "+1") smallG
-- FaceId 2
-- >>> rightFace (dart 2 "-1") smallG
-- FaceId 0
rightFace   :: Dart s -> PlaneGraph s v e f r  -> FaceId' s
rightFace d = PG.rightFace d . _graph


-- | Get the next edge along the face
--
--
-- running time: \(O(1)\).
--
-- >>> nextEdge (dart 1 "+1") smallG
-- Dart (Arc 4) +1
nextEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextEdge d = PG.nextEdge d . _graph

-- | Get the previous edge along the face
--
--
-- running time: \(O(1)\).
--
-- >>> prevEdge (dart 1 "+1") smallG
-- Dart (Arc 0) -1
prevEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevEdge d = PG.prevEdge d . _graph


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
--
--
boundary   :: FaceId' s -> PlaneGraph s v e f r  -> V.Vector (Dart s)
boundary f = PG.boundary f . _graph

-- | Generates the darts incident to a face, starting with the given dart.
--
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'   :: Dart s -> PlaneGraph s v e f r -> V.Vector (Dart s)
boundary' d = PG.boundary' d . _graph

-- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
-- the right of the dart.
boundaryDart   :: FaceId' s -> PlaneGraph s v e f r -> Dart s
boundaryDart f = PG.boundaryDart f . _graph

-- | The vertices bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices   :: FaceId' s -> PlaneGraph s v e f r
                   -> V.Vector (VertexId' s)
boundaryVertices f = PG.boundaryVertices f . _graph


--------------------------------------------------------------------------------
-- * Access data

vertexDataOf   :: VertexId' s -> Lens' (PlaneGraph s v e f r ) (VertexData r v)
vertexDataOf v = graph.PG.dataOf v

locationOf   :: VertexId' s -> Lens' (PlaneGraph s v e f r ) (Point 2 r)
locationOf v = vertexDataOf v.location


instance HasDataOf (PlaneGraph s v e f r) (VertexId' s) where
  type DataOf (PlaneGraph s v e f r) (VertexId' s) = v
  dataOf v = graph.dataOf v.vData

instance HasDataOf (PlaneGraph s v e f r) (Dart s) where
  type DataOf (PlaneGraph s v e f r) (Dart s) = e
  dataOf d = graph.dataOf d

instance HasDataOf (PlaneGraph s v e f r) (FaceId' s) where
  type DataOf (PlaneGraph s v e f r) (FaceId' s) = f
  dataOf f = graph.dataOf f


-- | Traverse the vertices
--
-- (^.vertexData) <$> traverseVertices (\i x -> Just (i,x)) smallG
-- Just [(VertexId 0,0),(VertexId 1,1),(VertexId 2,2),(VertexId 3,3)]
-- >>> traverseVertices (\i x -> print (i,x)) smallG >> pure ()
-- (VertexId 0,0)
-- (VertexId 1,1)
-- (VertexId 2,2)
-- (VertexId 3,3)
traverseVertices   :: Applicative m
                   => (VertexId' s -> v -> m v')
                   -> PlaneGraph s v e f r
                   -> m (PlaneGraph s v' e f r)
traverseVertices f = itraverseOf (vertexData.itraversed) (\i -> f (VertexId i))

-- | Traverses the darts
--
-- >>> traverseDarts (\d x -> print (d,x)) smallG >> pure ()
-- (Dart (Arc 0) +1,"0->2")
-- (Dart (Arc 0) -1,"2->0")
-- (Dart (Arc 1) +1,"0->1")
-- (Dart (Arc 1) -1,"1->0")
-- (Dart (Arc 2) +1,"0->3")
-- (Dart (Arc 2) -1,"3->0")
-- (Dart (Arc 3) +1,"1->3")
-- (Dart (Arc 3) -1,"3->1")
-- (Dart (Arc 4) +1,"1->2")
-- (Dart (Arc 4) -1,"2->1")
traverseDarts   :: Applicative m
                => (Dart s -> e -> m e')
                -> PlaneGraph s v e f r
                -> m (PlaneGraph s v e' f r)
traverseDarts f = traverseOf graph (PG.traverseDarts f)


-- | Traverses the faces
--
-- >>> traverseFaces (\i x -> print (i,x)) smallG >> pure ()
-- (FaceId 0,"OuterFace")
-- (FaceId 1,"A")
-- (FaceId 2,"B")
traverseFaces   :: Applicative m
                => (FaceId' s  -> f -> m f')
                -> PlaneGraph s v e f r
                -> m (PlaneGraph s v e f' r)
traverseFaces f = traverseOf graph (PG.traverseFaces f)


-- | Getter for the data at the endpoints of a dart
--
-- running time: \(O(1)\)
endPointsOf   :: Dart s -> Getter (PlaneGraph s v e f r )
                                  (VertexData r v, VertexData r v)
endPointsOf d = graph.PG.endPointDataOf d

-- | Data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData   :: Dart s -> PlaneGraph s v e f r
               ->  (VertexData r v, VertexData r v)
endPointData d = PG.endPointData d . _graph

--------------------------------------------------------------------------------

-- | gets the id of the outer face
--
-- running time: \(O(n)\)
--
outerFaceId    :: (Ord r, Fractional r) => PlaneGraph s v e f r -> FaceId' s
outerFaceId ps = leftFace (outerFaceDart ps) ps


-- | gets a dart incident to the outer face (in particular, that has the
-- outerface on its left)
--
-- running time: \(O(n)\)
--
outerFaceDart    :: (Ord r, Fractional r) => PlaneGraph s v e f r -> Dart s
outerFaceDart ps = d
  where
    (v,_)  = V.minimumBy (comparing (^._2.location)) . vertices $ ps
           -- compare lexicographically; i.e. if same x-coord prefer the one with the
           -- smallest y-coord
    d :+ _ = V.maximumBy (cmpSlope `on` (^.extra))
           .  fmap (\d' -> d' :+ (edgeSegment d' ps)^.core.to supportingLine)
           $ incidentEdges v ps
    -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
    -- basically: find the leftmost vertex, find the incident edge with the largest slope
    -- and take the face left of that edge. This is the outerface.
    -- note that this requires that the edges are straight line segments


--------------------------------------------------------------------------------
-- * Reporting Geometries

-- | Reports all edges as line segments
--
-- >>> mapM_ print $ edgeSegments smallG
-- (Dart (Arc 0) +1,LineSegment (Closed (Point2 [0,0] :+ 0)) (Closed (Point2 [2,0] :+ 2)) :+ "0->2")
-- (Dart (Arc 1) +1,LineSegment (Closed (Point2 [0,0] :+ 0)) (Closed (Point2 [2,2] :+ 1)) :+ "0->1")
-- (Dart (Arc 2) +1,LineSegment (Closed (Point2 [0,0] :+ 0)) (Closed (Point2 [-1,4] :+ 3)) :+ "0->3")
-- (Dart (Arc 4) +1,LineSegment (Closed (Point2 [2,2] :+ 1)) (Closed (Point2 [2,0] :+ 2)) :+ "1->2")
-- (Dart (Arc 3) +1,LineSegment (Closed (Point2 [2,2] :+ 1)) (Closed (Point2 [-1,4] :+ 3)) :+ "1->3")
edgeSegments    :: PlaneGraph s v e f r -> V.Vector (Dart s, LineSegment 2 v r :+ e)
edgeSegments ps = fmap withSegment . edges $ ps
  where
    withSegment (d,e) = let (p,q) = bimap vtxDataToExt vtxDataToExt
                                  $ ps^.endPointsOf d
                            seg   = ClosedLineSegment p q
                        in (d, seg :+ e)

-- | Given a dart and the graph constructs the line segment representing the
-- dart. The segment \(\overline{uv})\) is has \(u\) as its tail and \(v\) as
-- its head.
--
-- \(O(1)\)
edgeSegment      :: Dart s -> PlaneGraph s v e f r -> LineSegment 2 v r :+ e
edgeSegment d ps = seg :+ ps^.dataOf d
  where
    seg = let (p,q) = bimap vtxDataToExt vtxDataToExt $ ps^.endPointsOf d
          in ClosedLineSegment p q

-- | The polygon describing the face
--
-- runningtime: \(O(k)\), where \(k\) is the size of the face.
--
--
rawFaceBoundary      :: FaceId' s -> PlaneGraph s v e f r
                    -> SimplePolygon v r :+ f
rawFaceBoundary i ps = pg :+ (ps^.dataOf i)
  where
    pg = fromPoints . F.toList . fmap (\j -> ps^.graph.dataOf j.to vtxDataToExt)
       . boundaryVertices i $ ps

-- | Alias for rawFace Boundary
--
-- runningtime: \(O(k)\), where \(k\) is the size of the face.
rawFacePolygon :: FaceId' s -> PlaneGraph s v e f r -> SimplePolygon v r :+ f
rawFacePolygon = rawFaceBoundary

-- | Lists all faces of the plane graph.
rawFacePolygons    :: PlaneGraph s v e f r
                   -> V.Vector (FaceId' s, SimplePolygon v r :+ f)
rawFacePolygons ps = fmap (\i -> (i,rawFacePolygon i ps)) . faces' $ ps

--------------------------------------------------------------------------------

-- | Labels the edges of a plane graph with their distances, as specified by
-- the distance function.
withEdgeDistances     :: (Point 2 r ->  Point 2 r -> a)
                      -> PlaneGraph s p e f r -> PlaneGraph s p (a :+ e) f r
withEdgeDistances f g = g&graph.PG.dartData %~ fmap (\(d,x) -> (d,len d :+ x))
  where
    len d = uncurry f . over both (^.location) $ endPointData d g
