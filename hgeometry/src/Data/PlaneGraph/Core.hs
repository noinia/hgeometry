{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
                           , faces', internalFaces', faces, internalFaces, faces''
                           , darts', darts
                           , traverseVertices, traverseDarts, traverseFaces

                           , headOf, tailOf, twin, endPoints

                           , incidentEdges, incomingEdges, outgoingEdges
                           , neighboursOf
                           , nextIncidentEdge, prevIncidentEdge
                           , nextIncidentEdgeFrom, prevIncidentEdgeFrom


                           , leftFace, rightFace
                           , nextEdge, prevEdge
                           , boundary, boundary', boundaryDart, boundaryVertices
                           , outerFaceId, outerFaceDart

                           , vertexDataOf, locationOf, HasDataOf(..)

                           , endPointsOf, endPointData
                           , vertexData, faceData, dartData, rawDartData

                           , edgeSegment, edgeSegments
                           , faceBoundary, internalFacePolygon
                           , outerFacePolygon, outerFacePolygon'
                           , facePolygons, facePolygons', internalFacePolygons

                           , VertexId(..), FaceId(..), Dart, World(..), VertexId', FaceId'

                           , withEdgeDistances
                           -- , writePlaneGraph, readPlaneGraph
                           ) where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Ext
import qualified Data.Foldable as F

import           Data.Geometry.Box
import           Data.Geometry.Interval

import           Data.Geometry.LineSegment hiding (endPoints)
import           Data.Geometry.Point
import           Data.Geometry.Vector.VectorFamily
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Ord (comparing)
import           Data.PlanarGraph          (Arc (..), Dart (..), Direction (..), FaceId (..),
                                            FaceId', HasDataOf (..), PlanarGraph, VertexId (..),
                                            VertexId', World (..), dual, planarGraph, twin)
import qualified Data.PlanarGraph as PG
import           Data.Util
import qualified Data.Vector as V
import           Data.Vector.Circular (CircularVector)
import           GHC.Generics (Generic)


--------------------------------------------------------------------------------

-- $setup
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
--     smallG = fromAdjRep @() small
-- :}
--
--
-- This represents the following graph. Note that the graph is undirected, the
-- arrows are just to indicate what the Positive direction of the darts is.
--
-- ![myGraph](docs/Data/PlaneGraph/small.png)
--
--
-- Here is also a slightly larger example graph:
-- ![myGraph](docs/Data/PlaneGraph/planegraph.png)
--
-- >>> import Data.RealNumber.Rational
-- >>> data MyWorld
-- >>> :{
-- let myPlaneGraph :: PlaneGraph MyWorld Int () String (RealNumber 5)
--     myPlaneGraph = fromAdjRep @MyWorld myPlaneGraphAdjrep
--     myPlaneGraphAdjrep :: Gr (Vtx Int () (RealNumber 5)) (Face String)
--     myPlaneGraphAdjrep = Gr [ vtx 0 (Point2 0   0   ) [e 9, e 5, e 1, e 2]
--                             , vtx 1 (Point2 4   4   ) [e 0, e 5, e 12]
--                             , vtx 2 (Point2 3   7   ) [e 0, e 3]
--                             , vtx 3 (Point2 0   5   ) [e 4, e 2]
--                             , vtx 4 (Point2 3   8   ) [e 3, e 13]
--                             , vtx 5 (Point2 8   1   ) [e 0, e 6, e 8, e 1]
--                             , vtx 6 (Point2 6   (-1)) [e 5, e 9]
--                             , vtx 7 (Point2 9   (-1)) [e 8, e 11]
--                             , vtx 8 (Point2 12  1   ) [e 7, e 12, e 5]
--                             , vtx 9 (Point2 8   (-5)) [e 0, e 10, e 6]
--                             , vtx 10 (Point2 12 (-3)) [e 9, e 11]
--                             , vtx 11 (Point2 14 (-1)) [e 10, e 7]
--                             , vtx 12 (Point2 10 4   ) [e 1, e 8, e 13, e 14]
--                             , vtx 13 (Point2 9  6   ) [e 4, e 14, e 12]
--                             , vtx 14 (Point2 8  5   ) [e 13, e 12]
--                             ]
--                             [ Face (0,9) "OuterFace"
--                             , Face (0,5) "A"
--                             , Face (0,1) "B"
--                             , Face (0,2) "C"
--                             , Face (14,13) "D"
--                             , Face (1,12) "E"
--                             , Face (5,8) "F"
--                             ]
--       where
--         e i = (i,())
--         vtx i p es = Vtx i p es i
-- :}

--------------------------------------------------------------------------------
-- * Vertex Data

-- | Note that the functor instance is in v
data VertexData r v = VertexData { _location :: !(Point 2 r)
                                 , _vData    :: !v
                                 } deriving (Show,Eq,Ord,Generic
                                            ,Functor,Foldable,Traversable)
makeLenses ''VertexData

-- | Convert to an Ext
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
fromSimplePolygon                            :: forall s p r f.
                                                SimplePolygon p r
                                             -> f -- ^ data inside
                                             -> f -- ^ data outside the polygon
                                             -> PlaneGraph s p () f r
fromSimplePolygon poly iD oD = PlaneGraph g'
  where
    vs     = poly ^. outerBoundaryVector
    g      = fromVertices vs
    fData' = V.fromList [iD, oD]
    g'     = g & PG.faceData .~ fData'

-- | Constructs a planar from the given vertices
fromVertices    :: forall s r p.
                   CircularVector (Point 2 r :+ p)
                -> PlanarGraph s Primal (VertexData r p) () ()
fromVertices vs = g&PG.vertexData .~ vData'
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
fromConnectedSegments    :: forall s p r e f. (Foldable f, Ord r, Num r)
                         => f (LineSegment 2 p r :+ e)
                         -> PlaneGraph s (NonEmpty.NonEmpty p) e () r
fromConnectedSegments ss = PlaneGraph $ planarGraph dts & PG.vertexData .~ vxData
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

    vts    = map (\(p,sp) -> (p,map (^.extra) . sortAround' (ext p) <$> sp))
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
-- >>> numVertices myPlaneGraph
-- 15
numVertices :: PlaneGraph s v e f r  -> Int
numVertices = PG.numVertices . _graph

-- | Get the number of Darts
--
-- >>> numDarts smallG
-- 10
--
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
-- >>> numFaces myPlaneGraph
-- 7
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
-- (VertexId 0,VertexData {_location = Point2 0 0, _vData = 0})
-- (VertexId 1,VertexData {_location = Point2 2 2, _vData = 1})
-- (VertexId 2,VertexData {_location = Point2 2 0, _vData = 2})
-- (VertexId 3,VertexData {_location = Point2 (-1) 4, _vData = 3})
vertices   :: PlaneGraph s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices = PG.vertices . _graph

-- | Enumerate all darts
darts' :: PlaneGraph s v e f r  -> V.Vector (Dart s)
darts' = PG.darts' . _graph

-- | Get all darts together with their data
--
--
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


-- | face Ids of all internal faces in the plane graph
--
-- running time: \(O(n)\)
internalFaces'   :: (Ord r, Num r) => PlaneGraph s v e f r  -> V.Vector (FaceId' s)
internalFaces' g = let i = outerFaceId g in V.filter (/= i) $ faces' g

-- | All faces with their face data.
--
-- >>> mapM_ print $ faces smallG
-- (FaceId 0,"OuterFace")
-- (FaceId 1,"A")
-- (FaceId 2,"B")
-- >>> mapM_ print $ faces myPlaneGraph
-- (FaceId 0,"OuterFace")
-- (FaceId 1,"A")
-- (FaceId 2,"B")
-- (FaceId 3,"C")
-- (FaceId 4,"E")
-- (FaceId 5,"F")
-- (FaceId 6,"D")
faces :: PlaneGraph s v e f r  -> V.Vector (FaceId' s, f)
faces = PG.faces . _graph

-- | Reports the outerface and all internal faces separately.
-- running time: \(O(n)\)
faces''   :: (Ord r, Num r)
          => PlaneGraph s v e f r -> ((FaceId' s, f), V.Vector (FaceId' s, f))
faces'' g = let i = outerFaceId g
            in ((i,g^.dataOf i), V.filter (\(j,_) -> i /= j) $ faces g)

-- | Reports all internal faces.
-- running time: \(O(n)\)
internalFaces :: (Ord r, Num r)
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
-- >>> mapM_ print $ incidentEdges (VertexId 5) myPlaneGraph
-- Dart (Arc 1) -1
-- Dart (Arc 7) +1
-- Dart (Arc 10) +1
-- Dart (Arc 4) -1
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



-- | All edges incident to vertex v in outgoing direction
-- (i.e. pointing out of v) in counterclockwise order around v.
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
-- >>> neighboursOf (VertexId 5) myPlaneGraph
-- [VertexId 0,VertexId 6,VertexId 8,VertexId 1]
neighboursOf   :: VertexId' s -> PlaneGraph s v e f r
               -> V.Vector (VertexId' s)
neighboursOf v = PG.neighboursOf v . _graph

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
--
-- >>> nextIncidentEdge (dart 1 "+1") smallG
-- Dart (Arc 4) +1
-- >>> nextIncidentEdge (dart 1 "+1") myPlaneGraph
-- Dart (Arc 7) +1
-- >>> nextIncidentEdge (dart 17 "-1") myPlaneGraph
-- Dart (Arc 15) -1
nextIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextIncidentEdge d = PG.nextIncidentEdge d . _graph

-- | Given a dart d that points into some vertex v, report the previous dart in the
-- cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
--
-- >>> prevIncidentEdge (dart 1 "+1") smallG
-- Dart (Arc 3) +1
-- >>> prevIncidentEdge (dart 1 "+1") myPlaneGraph
-- Dart (Arc 4) -1
-- >>> prevIncidentEdge (dart 7 "-1") myPlaneGraph
-- Dart (Arc 1) -1
prevIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevIncidentEdge d = PG.prevIncidentEdge d . _graph


-- | Given a dart d that points away from some vertex v, report the
-- next dart in the cyclic (counterclockwise) order around v.
--
--
-- running time: \(O(1)\)
--
-- >>> nextIncidentEdgeFrom (dart 1 "+1") smallG
-- Dart (Arc 2) +1
-- >>> nextIncidentEdgeFrom (dart 1 "+1") myPlaneGraph
-- Dart (Arc 2) +1
-- >>> nextIncidentEdgeFrom (dart 4 "+1") myPlaneGraph
-- Dart (Arc 15) +1
nextIncidentEdgeFrom   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextIncidentEdgeFrom d = PG.nextIncidentEdgeFrom d . _graph

-- | Given a dart d that points into away from vertex v, report the previous dart in the
-- cyclic (counterclockwise) order around v.
--
-- running time: \(O(1)\)
--
-- >>> prevIncidentEdgeFrom (dart 1 "+1") smallG
-- Dart (Arc 0) +1
-- >>> prevIncidentEdgeFrom (dart 4 "+1") myPlaneGraph
-- Dart (Arc 2) -1
prevIncidentEdgeFrom   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevIncidentEdgeFrom d = PG.prevIncidentEdgeFrom d . _graph


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


-- | The darts bounding this face. The darts are reported in order
-- along the face. This means that for internal faces the darts are
-- reported in *clockwise* order along the boundary, whereas for the
-- outer face the darts are reported in counter clockwise order.
--
-- running time: \(O(k)\), where \(k\) is the output size.
--
-- >>> boundary (FaceId $ VertexId 2) smallG -- around face B
-- [Dart (Arc 2) +1,Dart (Arc 3) -1,Dart (Arc 1) -1]
-- >>> boundary (FaceId $ VertexId 0) smallG -- around outer face
-- [Dart (Arc 0) +1,Dart (Arc 4) -1,Dart (Arc 3) +1,Dart (Arc 2) -1]
--
boundary   :: FaceId' s -> PlaneGraph s v e f r  -> V.Vector (Dart s)
boundary f = PG.boundary f . _graph

-- | Given a dart d, generates the darts bounding the face that is to
-- the right of the given dart. The darts are reported in order along
-- the face. This means that for internal faces the darts are reported
-- in *clockwise* order along the boundary, whereas for the outer face
-- the darts are reported in counter clockwise order.
--
-- running time: \(O(k)\), where \(k\) is the number of darts reported
--
-- >>> boundary' (dart 2 "+1") smallG -- around face B
-- [Dart (Arc 2) +1,Dart (Arc 3) -1,Dart (Arc 1) -1]
-- >>> boundary' (dart 0 "+1") smallG -- around outer face
-- [Dart (Arc 0) +1,Dart (Arc 4) -1,Dart (Arc 3) +1,Dart (Arc 2) -1]
--
boundary'   :: Dart s -> PlaneGraph s v e f r -> V.Vector (Dart s)
boundary' d = PG.boundary' d . _graph

-- | Gets a dart bounding this face. I.e. a dart d such that the face lies to
-- the right of the dart.
boundaryDart   :: FaceId' s -> PlaneGraph s v e f r -> Dart s
boundaryDart f = PG.boundaryDart f . _graph

-- | The vertices bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
-- running time: \(O(k)\), where \(k\) is the output size.
--
-- >>> boundaryVertices (FaceId $ VertexId 2) smallG -- around B
-- [VertexId 0,VertexId 3,VertexId 1]
-- >>> boundaryVertices (FaceId $ VertexId 0) smallG -- around outerface
-- [VertexId 0,VertexId 2,VertexId 1,VertexId 3]
-- >>> mapM_ print $ boundaryVertices (FaceId $ VertexId 0) myPlaneGraph
-- VertexId 0
-- VertexId 9
-- VertexId 10
-- VertexId 11
-- VertexId 7
-- VertexId 8
-- VertexId 12
-- VertexId 13
-- VertexId 4
-- VertexId 3
-- VertexId 2
boundaryVertices   :: FaceId' s -> PlaneGraph s v e f r
                   -> V.Vector (VertexId' s)
boundaryVertices f = PG.boundaryVertices f . _graph


--------------------------------------------------------------------------------
-- * Access data


-- | Lens to access the vertex data
--
-- Note that using the setting part of this lens may be very
-- expensive!!  (O(n))
vertexDataOf   :: VertexId' s -> Lens' (PlaneGraph s v e f r ) (VertexData r v)
vertexDataOf v = graph.PG.dataOf v

-- | Get the location of a vertex in the plane graph
--
-- Note that the setting part of this lens may be very expensive!
-- Moreover, use with care (as this may destroy planarity etc.)
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
traverseVertices f = itraverseOf (vertexData.itraversed) (f . VertexId)

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
outerFaceId    :: (Ord r, Num r) => PlaneGraph s v e f r -> FaceId' s
outerFaceId ps = leftFace (outerFaceDart ps) ps


-- | gets a dart incident to the outer face (in particular, that has the
-- outerface on its left)
--
-- running time: \(O(n)\)
--
outerFaceDart    :: (Ord r, Num r) => PlaneGraph s v e f r -> Dart s
outerFaceDart pg = d
  where
    (v,_)  = V.minimumBy (comparing (^._2.location)) . vertices $ pg
           -- compare lexicographically; i.e. if same x-coord prefer the one with the
           -- smallest y-coord

    (_ :+ d) = V.minimumBy (cwCmpAroundWith' (Vector2 (-1) 0) (pg^.locationOf v :+ ()))
             . fmap (\d' -> let u = headOf d' pg in (pg^.locationOf u) :+ d')
             $ outgoingEdges v pg
    -- based on the approach sketched at https://cstheory.stackexchange.com/questions/27586/finding-outer-face-in-plane-graph-embedded-planar-graph
    -- basically: find the leftmost vertex, find the incident edge with the largest slope
    -- and take the face left of that edge. This is the outerface.
    -- note that this requires that the edges are straight line segments
    --
    -- note that rather computing slopes we just ask for the first
    -- vertec cw vertex around v. First with respect to some direction
    -- pointing towards the left.


--------------------------------------------------------------------------------
-- * Reporting Geometries

-- | Reports all edges as line segments
--
-- >>> mapM_ print $ edgeSegments smallG
-- (Dart (Arc 0) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 2 0 :+ 2) :+ "0->2")
-- (Dart (Arc 1) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 2 2 :+ 1) :+ "0->1")
-- (Dart (Arc 2) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 (-1) 4 :+ 3) :+ "0->3")
-- (Dart (Arc 4) +1,ClosedLineSegment (Point2 2 2 :+ 1) (Point2 2 0 :+ 2) :+ "1->2")
-- (Dart (Arc 3) +1,ClosedLineSegment (Point2 2 2 :+ 1) (Point2 (-1) 4 :+ 3) :+ "1->3")
-- >>> mapM_ print $ edgeSegments myPlaneGraph
-- (Dart (Arc 0) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 8 (-5) :+ 9) :+ ())
-- (Dart (Arc 1) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 8 1 :+ 5) :+ ())
-- (Dart (Arc 2) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 4 4 :+ 1) :+ ())
-- (Dart (Arc 3) +1,ClosedLineSegment (Point2 0 0 :+ 0) (Point2 3 7 :+ 2) :+ ())
-- (Dart (Arc 4) +1,ClosedLineSegment (Point2 4 4 :+ 1) (Point2 8 1 :+ 5) :+ ())
-- (Dart (Arc 15) +1,ClosedLineSegment (Point2 4 4 :+ 1) (Point2 10 4 :+ 12) :+ ())
-- (Dart (Arc 5) +1,ClosedLineSegment (Point2 3 7 :+ 2) (Point2 0 5 :+ 3) :+ ())
-- (Dart (Arc 6) +1,ClosedLineSegment (Point2 0 5 :+ 3) (Point2 3 8 :+ 4) :+ ())
-- (Dart (Arc 18) +1,ClosedLineSegment (Point2 3 8 :+ 4) (Point2 9 6 :+ 13) :+ ())
-- (Dart (Arc 7) +1,ClosedLineSegment (Point2 8 1 :+ 5) (Point2 6 (-1) :+ 6) :+ ())
-- (Dart (Arc 10) +1,ClosedLineSegment (Point2 8 1 :+ 5) (Point2 12 1 :+ 8) :+ ())
-- (Dart (Arc 12) +1,ClosedLineSegment (Point2 6 (-1) :+ 6) (Point2 8 (-5) :+ 9) :+ ())
-- (Dart (Arc 8) +1,ClosedLineSegment (Point2 9 (-1) :+ 7) (Point2 12 1 :+ 8) :+ ())
-- (Dart (Arc 14) +1,ClosedLineSegment (Point2 9 (-1) :+ 7) (Point2 14 (-1) :+ 11) :+ ())
-- (Dart (Arc 9) +1,ClosedLineSegment (Point2 12 1 :+ 8) (Point2 10 4 :+ 12) :+ ())
-- (Dart (Arc 11) +1,ClosedLineSegment (Point2 8 (-5) :+ 9) (Point2 12 (-3) :+ 10) :+ ())
-- (Dart (Arc 13) +1,ClosedLineSegment (Point2 12 (-3) :+ 10) (Point2 14 (-1) :+ 11) :+ ())
-- (Dart (Arc 16) +1,ClosedLineSegment (Point2 10 4 :+ 12) (Point2 9 6 :+ 13) :+ ())
-- (Dart (Arc 17) +1,ClosedLineSegment (Point2 10 4 :+ 12) (Point2 8 5 :+ 14) :+ ())
-- (Dart (Arc 19) +1,ClosedLineSegment (Point2 9 6 :+ 13) (Point2 8 5 :+ 14) :+ ())
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


-- | The boundary of the face as a simple polygon. For internal faces
-- the polygon that is reported has its vertices stored in CCW order
-- (as expected).
--
-- pre: FaceId refers to an internal face.
--
-- For the other face this prodcuces a polygon in CW order (this may
-- lead to unexpected results.)
--
-- runningtime: \(O(k)\), where \(k\) is the size of the face.
faceBoundary      :: FaceId' s -> PlaneGraph s v e f r -> SimplePolygon v r :+ f
faceBoundary i ps = pg :+ (ps^.dataOf i)
  where
    pg = unsafeFromVector . V.reverse . fmap (\j -> ps^.graph.dataOf j.to vtxDataToExt)
       . boundaryVertices i $ ps
    -- polygons are stored in CCW order, the boundaryVertices of
    -- internal faces are reported in CW order we reverse them.

--------------------------------------------------------------------------------

-- | The boundary of the face as a simple polygon. For internal faces
-- the polygon that is reported has its vertices stored in CCW order
-- (as expected).
--
-- pre: FaceId refers to an internal face.
--
-- For the other face this prodcuces a polygon in CW order (this may
-- lead to unexpected results.)
--
-- runningtime: \(O(k)\), where \(k\) is the size of the face.
internalFacePolygon :: FaceId' s -> PlaneGraph s v e f r -> SimplePolygon v r :+ f
internalFacePolygon = faceBoundary

-- | Given the outerFaceId and the graph, construct a sufficiently
-- large rectangular multipolygon ith a hole containing the boundary
-- of the outer face.
outerFacePolygon      :: (Num r, Ord r)
                       => FaceId' s -> PlaneGraph s v e f r -> MultiPolygon (Maybe v) r :+ f
outerFacePolygon i pg =
    outerFacePolygon' i outer pg & core %~ first (either (const Nothing) Just)
  where
    outer = rectToPolygon . grow 1 . boundingBox $ pg
    rectToPolygon = unsafeFromPoints . reverse . F.toList . corners

-- | Given the outerface id, and a sufficiently large outer boundary,
-- draw the outerface as a polygon with a hole.
outerFacePolygon'            :: FaceId' s -> SimplePolygon v' r
                             -> PlaneGraph s v e f r -> MultiPolygon (Either v' v) r :+ f
outerFacePolygon' i outer pg = MultiPolygon (first Left outer) [hole] :+ pg^.dataOf i
  where
    hole = reverseOuterBoundary . first Right . view core $ faceBoundary i pg
    -- if we call faceBoundary on the outerface we get a polygon in
    -- the wrong orientation. So reverse it.

--------------------------------------------------------------------------------

-- | Given the outerFace Id, construct polygons for all faces. We
-- construct a polygon with a hole for the outer face.
--
facePolygons      :: (Num r, Ord r) => FaceId' s -> PlaneGraph s v e f r
                  -> ( (FaceId' s, MultiPolygon (Maybe v) r :+ f)
                     , V.Vector (FaceId' s, SimplePolygon v r :+ f)
                     )
facePolygons i ps = ((i, outerFacePolygon i ps), facePolygons' i ps)

-- | Given the outerFace Id, lists all internal faces of the plane
-- graph with their boundaries.
facePolygons'      :: FaceId' s -> PlaneGraph s v e f r
                   ->  V.Vector (FaceId' s, SimplePolygon v r :+ f)
facePolygons' i ps = fmap (\j -> (j,internalFacePolygon j ps)) . V.filter (/= i) . faces' $ ps


-- | lists all internal faces of the plane graph with their
-- boundaries.
internalFacePolygons    :: (Ord r, Num r)
                        => PlaneGraph s v e f r ->  V.Vector (FaceId' s, SimplePolygon v r :+ f)
internalFacePolygons pg = facePolygons' (outerFaceId pg) pg

--------------------------------------------------------------------------------

-- | Labels the edges of a plane graph with their distances, as specified by
-- the distance function.
withEdgeDistances     :: (Point 2 r ->  Point 2 r -> a)
                      -> PlaneGraph s p e f r -> PlaneGraph s p (a :+ e) f r
withEdgeDistances f g = g&graph.PG.dartData %~ fmap (\(d,x) -> (d,len d :+ x))
  where
    len d = uncurry f . over both (^.location) $ endPointData d g



--------------------------------------------------------------------------------
