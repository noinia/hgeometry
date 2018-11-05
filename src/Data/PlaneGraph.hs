{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module Data.PlaneGraph( PlaneGraph(PlaneGraph), graph
                      , PlanarGraph
                      , VertexData(VertexData), vData, location, vtxDataToExt
                      , fromSimplePolygon, fromConnectedSegments
                      , PG.fromAdjacencyLists

                      , numVertices, numEdges, numFaces, numDarts
                      , dual

                      , vertices', vertices
                      , edges', edges
                      , faces', faces, internalFaces, faces''
                      , darts'
                      , traverseVertices, traverseDarts, traverseFaces

                      , headOf, tailOf, twin, endPoints

                      , incidentEdges, incomingEdges, outgoingEdges
                      , neighboursOf
                      , nextIncidentEdge, prevIncidentEdge


                      , leftFace, rightFace
                      , nextEdge, prevEdge
                      , boundary, boundary', boundaryVertices
                      , outerFaceId, outerFaceDart

                      , vertexDataOf, locationOf, HasDataOf(..)

                      , endPointsOf, endPointData
                      , vertexData, faceData, dartData, rawDartData

                      , edgeSegment, edgeSegments
                      , rawFacePolygon, rawFaceBoundary
                      , rawFacePolygons

                      , VertexId(..), FaceId(..), Dart, World(..), VertexId', FaceId'


                      , withEdgeDistances
                      , writePlaneGraph, readPlaneGraph
                      ) where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.Line (cmpSlope, supportingLine)
import           Data.Geometry.LineSegment
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
import           Data.Version
import           Data.Yaml (ParseException)
import           Data.Yaml.Util
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

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

-- | Embedded, *connected*, planar graph
newtype PlaneGraph s v e f r =
    PlaneGraph { _graph :: PlanarGraph s Primal (VertexData r v) e f }
      deriving (Show,Eq,ToJSON,FromJSON,Generic)
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
numVertices :: PlaneGraph s v e f r  -> Int
numVertices = PG.numVertices . _graph

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: PlaneGraph s v e f r  -> Int
numDarts = PG.numDarts . _graph

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: PlaneGraph s v e f r  -> Int
numEdges = PG.numEdges . _graph

-- | Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces :: PlaneGraph s v e f r  -> Int
numFaces = PG.numFaces . _graph

-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlaneGraph s v e f r  -> V.Vector (VertexId' s)
vertices' = PG.vertices' . _graph

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices   :: PlaneGraph s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices = PG.vertices . _graph

-- | Enumerate all darts
darts' :: PlaneGraph s v e f r  -> V.Vector (Dart s)
darts' = PG.darts' . _graph

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
-- >>> mapM_ print $ edges myGraph
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 3) +1,"d+")
edges :: PlaneGraph s v e f r  -> V.Vector (Dart s, e)
edges = PG.edges . _graph

-- | Enumerate all faces in the plane graph
faces' :: PlaneGraph s v e f r  -> V.Vector (FaceId' s)
faces' = PG.faces' . _graph

-- | All faces with their face data.
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
tailOf   :: Dart s -> PlaneGraph s v e f r  -> VertexId' s
tailOf d = PG.tailOf d . _graph

-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
headOf   :: Dart s -> PlaneGraph s v e f r  -> VertexId' s
headOf d = PG.headOf d . _graph

-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
endPoints   :: Dart s -> PlaneGraph s v e f r
            -> (VertexId' s, VertexId' s)
endPoints d = PG.endPoints d . _graph

-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k\) is the output size
incidentEdges   :: VertexId' s -> PlaneGraph s v e f r -> V.Vector (Dart s)
incidentEdges v = PG.incidentEdges v . _graph

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges   :: VertexId' s -> PlaneGraph s v e f r -> V.Vector (Dart s)
incomingEdges v = PG.incomingEdges v . _graph

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges   :: VertexId' s -> PlaneGraph s v e f r  -> V.Vector (Dart s)
outgoingEdges v = PG.outgoingEdges v . _graph

-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf   :: VertexId' s -> PlaneGraph s v e f r
               -> V.Vector (VertexId' s)
neighboursOf v = PG.neighboursOf v . _graph

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v.
--
-- running time: \(O(1)\)
nextIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextIncidentEdge d = PG.nextIncidentEdge d . _graph

-- | Given a dart d that points into some vertex v, report the next dart in the
-- cyclic order around v.
--
-- running time: \(O(1)\)
prevIncidentEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevIncidentEdge d = PG.prevIncidentEdge d . _graph


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
leftFace   :: Dart s -> PlaneGraph s v e f r  -> FaceId' s
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
rightFace   :: Dart s -> PlaneGraph s v e f r  -> FaceId' s
rightFace d = PG.rightFace d . _graph


-- | Get the next edge along the face
--
--
-- running time: \(O(1)\).
nextEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
nextEdge d = PG.nextEdge d . _graph

-- | Get the previous edge along the face
--
--
-- running time: \(O(1)\).
prevEdge   :: Dart s -> PlaneGraph s v e f r -> Dart s
prevEdge d = PG.prevEdge d . _graph


-- | The darts bounding this face, for internal faces in clockwise order, for
-- the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundary   :: FaceId' s -> PlaneGraph s v e f r  -> V.Vector (Dart s)
boundary f = PG.boundary f . _graph

-- | Generates the darts incident to a face, starting with the given dart.
--
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'   :: Dart s -> PlaneGraph s v e f r -> V.Vector (Dart s)
boundary' d = PG.boundary' d . _graph


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
-- (^.vertexData) <$> traverseVertices (\i x -> Just (i,x)) myGraph
-- Just [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
-- >>> traverseVertices (\i x -> print (i,x)) myGraph >> pure ()
-- (VertexId 0,())
-- (VertexId 1,())
-- (VertexId 2,())
-- (VertexId 3,())
traverseVertices   :: Applicative m
                   => (VertexId' s -> v -> m v')
                   -> PlaneGraph s v e f r
                   -> m (PlaneGraph s v' e f r)
traverseVertices f = itraverseOf (vertexData.itraversed) (\i -> f (VertexId i))

-- | Traverses the darts
--
-- >>> traverseDarts (\d x -> print (d,x)) myGraph >> pure ()
-- (Dart (Arc 0) +1,"a+")
-- (Dart (Arc 0) -1,"a-")
-- (Dart (Arc 1) +1,"b+")
-- (Dart (Arc 1) -1,"b-")
-- (Dart (Arc 2) +1,"c+")
-- (Dart (Arc 2) -1,"c-")
-- (Dart (Arc 3) +1,"d+")
-- (Dart (Arc 3) -1,"d-")
-- (Dart (Arc 4) +1,"e+")
-- (Dart (Arc 4) -1,"e-")
-- (Dart (Arc 5) +1,"g+")
-- (Dart (Arc 5) -1,"g-")
traverseDarts   :: Applicative m
                => (Dart s -> e -> m e')
                -> PlaneGraph s v e f r
                -> m (PlaneGraph s v e' f r)
traverseDarts f = traverseOf graph (PG.traverseDarts f)


-- | Traverses the faces
--
-- >>> traverseFaces (\i x -> print (i,x)) myGraph >> pure ()
-- (FaceId 0,())
-- (FaceId 1,())
-- (FaceId 2,())
-- (FaceId 3,())
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
outerFaceId    :: (Ord r, Fractional r) => PlaneGraph s v e f r -> FaceId' s
outerFaceId ps = leftFace (outerFaceDart ps) ps


-- | gets a dart incident to the outer face (in particular, that has the
-- outerface on its left)
--
-- running time: \(O(n)\)
outerFaceDart    :: (Ord r, Fractional r) => PlaneGraph s v e f r -> Dart s
outerFaceDart ps = d
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

-- | Reports all edges as line segments
edgeSegments    :: PlaneGraph s v e f r -> V.Vector (Dart s, LineSegment 2 v r :+ e)
edgeSegments ps = fmap withSegment . edges $ ps
  where
    withSegment (d,e) = let (p,q) = bimap vtxDataToExt vtxDataToExt
                                  $ ps^.endPointsOf d
                            seg   = ClosedLineSegment p q
                        in (d, seg :+ e)

-- | Given a dart and the graph constructs the line segment representing the dart
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
-- * Reading and Writing the Plane Graph

-- | Reads a plane graph from a bytestring
readPlaneGraph   :: (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
                 => proxy s -> B.ByteString
                 -> Either ParseException (PlaneGraph s v e f r)
readPlaneGraph _ = decodeYaml

-- | Writes a plane graph to a bytestring
writePlaneGraph :: (ToJSON v, ToJSON e, ToJSON f, ToJSON r)
                => PlaneGraph s v e f r -> B.ByteString
writePlaneGraph = encodeYaml . Versioned planeGraphVersion


planeGraphVersion :: Version
planeGraphVersion = makeVersion [1,0]

--------------------------------------------------------------------------------

-- | Labels the edges of a plane graph with their distances, as specified by
-- the distance function.
withEdgeDistances     :: (Point 2 r ->  Point 2 r -> a)
                      -> PlaneGraph s p e f r -> PlaneGraph s p (a :+ e) f r
withEdgeDistances f g = g&graph.PG.dartData %~ fmap (\(d,x) -> (d,len d :+ x))
  where
    len d = uncurry f . over both (^.location) $ endPointData d g
