{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.PlanarSubdivision.Basic( VertexId', FaceId'
                                           , VertexData(VertexData), PG.vData, PG.location

                                           , FaceData(FaceData), holes, fData

                                           , PlanarSubdivision(PlanarSubdivision)
                                           , Wrap

                                           , Component, ComponentId

                                           , PolygonFaceData(..)
                                           , PlanarGraph
                                           , PlaneGraph
                                           , fromSimplePolygon
                                           , fromConnectedSegments
                                           , fromPlaneGraph, fromPlaneGraph'

                                           , numVertices, numEdges, numFaces, numDarts
                                           , dual

                                           , components, component
                                           , vertices', vertices
                                           , edges', edges
                                           , faces', faces, internalFaces
                                           , darts'

                                           , headOf, tailOf, twin, endPoints

                                           , incidentEdges, incomingEdges, outgoingEdges
                                           , nextIncidentEdge
                                           , neighboursOf

                                           , leftFace, rightFace
                                           , outerBoundaryDarts, boundaryVertices, holesOf
                                           , outerFaceId
                                           , boundary'

                                           , locationOf
                                           , HasDataOf(..)

                                           , endPointsOf, endPointData

                                           , edgeSegment, edgeSegments
                                           , rawFacePolygon, rawFaceBoundary
                                           , rawFacePolygons

                                           , VertexId(..), FaceId(..), Dart, World(..)


                                           , rawVertexData, rawDartData, rawFaceData
                                           , dataVal

                                           , dartMapping, Raw(..)
                                           ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Aeson
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Permutation (ix')
import           Data.PlanarGraph (toAdjacencyLists,buildFromJSON, isPositive, allDarts)
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph( PlaneGraph, PlanarGraph, dual
                                , Dart, VertexId(..), FaceId(..), twin
                                , World(..)
                                , VertexId', FaceId'
                                , VertexData, location, vData
                                , HasDataOf(..)
                                )
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _holes :: (Seq.Seq h)
                             , _fData :: !f
                             } deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)
makeLenses ''FaceData

instance Bifunctor FaceData where
  bimap f g (FaceData hs x) = FaceData (fmap f hs) (g x)


instance (FromJSON h, FromJSON f) => FromJSON (FaceData h f)
instance (ToJSON h, ToJSON f)     => ToJSON (FaceData h f) where
  toEncoding = genericToEncoding defaultOptions


--------------------------------------------------------------------------------


data Wrap' s
type family Wrap (s :: k) :: k where
  Wrap s = Wrap' s

newtype ComponentId s = ComponentId { unCI :: Int }
  deriving (Show,Eq,Ord,Generic,Bounded,Enum)


data Raw s ia a = Raw { _compId  :: {-# UNPACK #-} !(ComponentId s)
                      , _idxVal  :: {-# UNPACK #-} !ia
                      , _dataVal :: !a
                      } deriving (Eq,Show,Functor,Foldable,Traversable)
makeLenses ''Raw



--------------------------------------------------------------------------------

-- | A connected component.
--
-- For every face f, and every hole in this face, the facedata points to a dart
-- d on the hole s.t. this dart has the face f on its left. i.e.
-- leftFace d = f
type Component s r = PlaneGraph (Wrap s)
                                (VertexId' s) (Dart s) (FaceData (Dart s) (FaceId' s))
                                r


-- | A planarsubdivision is essentially a bunch of plane-graphs; one for every
-- connected component. These graphs store the global ID's (darts, vertexId's, faceId's)
-- in their data values. This essentially gives us a mapping between the two.
--
-- note that a face may actually occur in multiple graphs, hence when we store
-- the edges to the the holes, we store the global edgeId's rather than the
-- 'local' edgeId (dart)'s.
--
-- invariant: the outerface has faceId 0
data PlanarSubdivision s v e f r =
  PlanarSubdivision { _components    :: V.Vector (Component s r)
                    , _rawVertexData :: V.Vector (Raw s (VertexId' (Wrap s)) v)
                    , _rawDartData   :: V.Vector (Raw s (Dart      (Wrap s)) e)
                    , _rawFaceData   :: V.Vector (Raw s (FaceId'   (Wrap s)) f)
                    } deriving (Show,Eq,Functor)
makeLenses ''PlanarSubdivision


type instance NumType   (PlanarSubdivision s v e f r) = r
type instance Dimension (PlanarSubdivision s v e f r) = 2

instance IsBoxable (PlanarSubdivision s v e f r) where
  boundingBox = boundingBoxList' . V.toList . _components


component    :: ComponentId s -> Lens' (PlanarSubdivision s v e f r)
                                       (Component s r)
component ci = components.ix' (unCI ci)

--------------------------------------------------------------------------------

-- | Constructs a planarsubdivision from a PlaneGraph
--
-- runningTime: \(O(n)\)
fromPlaneGraph   :: forall s v e f r. (Ord r, Fractional r)
                      => PlaneGraph s v e f r -> PlanarSubdivision s v e f r
fromPlaneGraph g = fromPlaneGraph' g (PG.outerFaceDart g)

-- | Given a (connected) PlaneGraph and a dart that has the outerface on its left
-- | Constructs a planarsubdivision
--
-- runningTime: \(O(n)\)
fromPlaneGraph'        :: forall s v e f r. PlaneGraph s v e f r -> Dart s
                       -> PlanarSubdivision s v e f r
fromPlaneGraph' g ofD = PlanarSubdivision (V.singleton . coerce $ g') vd ed fd
  where
    c = ComponentId 0
    vd = V.imap    (\i v   -> Raw c (VertexId i) v)             $ g^.PG.vertexData
    ed = V.zipWith (\d dd  -> Raw c d            dd) allDarts'' $ g^.PG.rawDartData
    fd = V.imap (\i f      -> Raw c (mkFaceId i) f) . swapOf    $ g^.PG.faceData

    g' :: PlaneGraph s (VertexId' s) (Dart s) (FaceData (Dart s) (FaceId' s)) r
    g' = g&PG.faceData    %~ V.imap (\i _ -> mkFaceData i)
          &PG.vertexData  %~ V.imap (\i _ -> VertexId i)
          &PG.rawDartData .~ allDarts''

    allDarts'' :: forall s'. V.Vector (Dart s')
    allDarts'' = allDarts' (PG.numDarts g)

    -- make sure the outerFaceId is 0
    (FaceId (VertexId of')) = PG.leftFace ofD g

    -- at index i we are storing the outerface
    mkFaceData i | i == of'  = faceData (Seq.singleton ofD) 0
                 | i == 0    = faceData mempty of'
                 | otherwise = faceData mempty i
    faceData xs i = FaceData xs (FaceId . VertexId $ i)


    mkFaceId :: forall s'. Int -> FaceId' s'
    mkFaceId = FaceId . VertexId . mkFaceId'
    mkFaceId' i | i == 0    = of'
                | i == of'  = 0
                | otherwise = i
    swapOf = V.modify (\v -> MV.swap v 0 of')





-- | Construct a planar subdivision from a simple polygon
--
-- running time: \(O(n)\).
fromSimplePolygon            :: (Ord r, Fractional r)
                             => proxy s
                             -> SimplePolygon p r
                             -> f -- ^ data inside
                             -> f -- ^ data outside the polygon
                             -> PlanarSubdivision s p () f r
fromSimplePolygon p pg iD oD =
  fromPlaneGraph (PG.fromSimplePolygon p pg iD oD)

-- | Constructs a connected planar subdivision.
--
-- pre: the segments form a single connected component
-- running time: \(O(n\log n)\)
fromConnectedSegments    :: (Foldable f, Ord r, Fractional r)
                         => proxy s
                         -> f (LineSegment 2 p r :+ e)
                         -> PlanarSubdivision s (NonEmpty p) e () r
fromConnectedSegments px = fromPlaneGraph . PG.fromConnectedSegments px

--------------------------------------------------------------------------------

-- | Data type that expresses whether or not we are inside or outside the
-- polygon.
data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)


--------------------------------------------------------------------------------
-- * Basic Graph information

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 4
numVertices :: PlanarSubdivision s v e f r  -> Int
numVertices = V.length . _rawVertexData

-- | Get the number of Darts
--
-- >>> numDarts myGraph
-- 12
numDarts :: PlanarSubdivision s v e f r  -> Int
numDarts = V.length . _rawDartData

-- | Get the number of Edges
--
-- >>> numEdges myGraph
-- 6
numEdges :: PlanarSubdivision s v e f r  -> Int
numEdges = (`div` 2) . V.length . _rawDartData

-- | Get the number of faces
--
-- >>> numFaces myGraph
-- 4
numFaces :: PlanarSubdivision s v e f r  -> Int
numFaces = V.length . _rawFaceData

-- | Enumerate all vertices
--
-- >>> vertices' myGraph
-- [VertexId 0,VertexId 1,VertexId 2,VertexId 3]
vertices'   :: PlanarSubdivision s v e f r  -> V.Vector (VertexId' s)
vertices' ps = let n = numVertices ps
               in V.fromList $ map VertexId [0..n-1]

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices    :: PlanarSubdivision s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices ps = (\vi -> (vi,ps^.vertexDataOf vi)) <$> vertices' ps

-- | Enumerate all darts
darts' :: PlanarSubdivision s v e f r  -> V.Vector (Dart s)
darts' = allDarts' . numDarts

allDarts'   :: forall s'. Int -> V.Vector (Dart s')
allDarts' n = V.fromList $ take n allDarts


-- | Enumerate all edges. We report only the Positive darts
edges' :: PlanarSubdivision s v e f r  -> V.Vector (Dart s)
edges' = V.filter isPositive . darts'

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
edges    :: PlanarSubdivision s v e f r  -> V.Vector (Dart s, e)
edges ps = (\e -> (e,ps^.dataOf e)) <$> edges' ps


faces'    :: PlanarSubdivision s v e f r -> V.Vector (FaceId' s)
faces' ps = let n = numFaces ps
            in V.fromList $ map (FaceId . VertexId) [0..n-1]

faces    :: PlanarSubdivision s v e f r -> V.Vector (FaceId' s, FaceData (Dart s) f)
faces ps = (\fi -> (fi,ps^.faceDataOf fi)) <$> faces' ps

-- | Enumerates all faces with their face data exlcluding  the outer face
internalFaces    :: (Ord r, Fractional r)
                 => PlanarSubdivision s v e f r
                 -> V.Vector (FaceId' s, FaceData (Dart s) f)
internalFaces ps = let i = outerFaceId ps
                   in V.filter (\(j,_) -> i /= j) $ faces ps


-- -- | lens to access the Dart Data
-- dartData :: Lens (PlanarSubdivision s v e f r) (PlanarSubdivision s v e' f r)
--                  (V.Vector (Dart s, e)) (V.Vector (Dart s, e'))
-- dartData = graph.PG.dartData





-- | The tail of a dart, i.e. the vertex this dart is leaving from
--
-- running time: \(O(1)\)
tailOf      :: Dart s -> PlanarSubdivision s v e f r  -> VertexId' s
tailOf d ps = let (_,d',g) = asLocalD d ps
              in g^.dataOf (PG.tailOf d' g)


-- | The vertex this dart is heading in to
--
-- running time: \(O(1)\)
headOf       :: Dart s -> PlanarSubdivision s v e f r  -> VertexId' s
headOf d ps = let (_,d',g) = asLocalD d ps
              in g^.dataOf (PG.headOf d' g)


-- | endPoints d g = (tailOf d g, headOf d g)
--
-- running time: \(O(1)\)
endPoints      :: Dart s -> PlanarSubdivision s v e f r
               -> (VertexId' s, VertexId' s)
endPoints d ps = (tailOf d ps, headOf d ps)


-- | All edges incident to vertex v, in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k\) is the number of edges reported.
incidentEdges                 :: VertexId' s -> PlanarSubdivision s v e f r
                              -> V.Vector (Dart s)
incidentEdges v ps=  let (_,v',g) = asLocalV v ps
                         ds       = PG.incidentEdges v' g
                     in (\d -> g^.dataOf d) <$> ds


-- | Given a dart d that points into some vertex v, report the next
-- dart e in the cyclic order around v.
--
-- running time: \(O(1)\)
nextIncidentEdge      :: Dart s -> PlanarSubdivision s v e f r -> Dart s
nextIncidentEdge d ps = let (_,d',g) = asLocalD d ps
                            d''      = PG.nextIncidentEdge d' g
                        in g^.dataOf d''

-- | All incoming edges incident to vertex v, in counterclockwise order around v.
incomingEdges      :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (Dart s)
incomingEdges v ps = V.filter (not . isPositive) $ incidentEdges v ps

-- | All outgoing edges incident to vertex v, in counterclockwise order around v.
outgoingEdges      :: VertexId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
outgoingEdges v ps = V.filter isPositive $ incidentEdges v ps


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf      :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (VertexId' s)
neighboursOf v ps = otherVtx <$> incidentEdges v ps
  where
    otherVtx d = let u = tailOf d ps in if u == v then headOf d ps else u


-- | The face to the left of the dart
--
-- running time: \(O(1)\).
leftFace      :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
leftFace d ps = let (_,d',g) = asLocalD d ps
                    fi       = PG.leftFace d' g
                in g^.dataOf fi.fData

-- | The face to the right of the dart
--
-- running time: \(O(1)\).
rightFace      :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
rightFace d ps = let (_,d',g) = asLocalD d ps
                     fi       = PG.rightFace d' g
                in g^.dataOf fi.fData

-- | The darts on the outer boundary of the face, for internal faces in
-- clockwise order, for the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
outerBoundaryDarts      :: FaceId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
outerBoundaryDarts f ps = let (_,f',g) = asLocalF f ps
                              ds       = PG.boundary f' g
                          in (\d -> g^.dataOf d) <$> ds



-- | The vertices of the outer boundary of the face, for internal faces in
-- clockwise order, for the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices      :: FaceId' s -> PlanarSubdivision s v e f r
                      -> V.Vector (VertexId' s)
boundaryVertices f ps = (\d -> headOf d ps) <$> outerBoundaryDarts f ps


-- | Lists the holes in this face, given as a list of darts to arbitrary darts
-- on those faces.
--
-- running time: \(O(k)\), where \(k\) is the number of darts returned.
holesOf      :: FaceId' s -> PlanarSubdivision s v e f r -> Seq.Seq (Dart s)
holesOf f ps = ps^.faceDataOf f.holes


--------------------------------------------------------------------------------
-- * Access data



asLocalD      :: Dart s -> PlanarSubdivision s v e f r
              -> (ComponentId s, Dart (Wrap s), Component s r)
asLocalD d ps = let (Raw ci d' _) = ps^.rawDartData.ix' (fromEnum d)
                in (ci,d',ps^.component ci)




asLocalV                 :: VertexId' s -> PlanarSubdivision s v e f r
                         -> (ComponentId s, VertexId' (Wrap s), Component s r)
asLocalV (VertexId v) ps = let (Raw ci v' _) = ps^.rawVertexData.ix' v
                           in (ci,v',ps^.component ci)

asLocalF                          :: FaceId' s -> PlanarSubdivision s v e f r
                                  -> (ComponentId s, FaceId' (Wrap s), Component s r)
asLocalF (FaceId (VertexId f)) ps = let (Raw ci f' _) = ps^.rawFaceData.ix' f
                                    in (ci,f',ps^.component ci)



-- | Note that using the setting part of this lens may be very expensive!!
vertexDataOf               :: VertexId' s
                           -> Lens' (PlanarSubdivision s v e f r ) (VertexData r v)
vertexDataOf (VertexId vi) = lens get' set''
  where
    get' ps = let (Raw ci wvdi x) = ps^.rawVertexData.ix' vi
                  vd              = ps^.component ci.PG.vertexDataOf wvdi
              in vd&vData .~ x
    set'' ps x = let (Raw ci wvdi _)  = ps^.rawVertexData.ix' vi
                 in ps&rawVertexData.ix' vi.dataVal               .~ (x^.vData)
                      &component ci.PG.vertexDataOf wvdi.location .~ (x^.location)

locationOf   :: VertexId' s -> Lens' (PlanarSubdivision s v e f r ) (Point 2 r)
locationOf v = vertexDataOf v.location


faceDataOf    :: FaceId' s -> Lens' (PlanarSubdivision s v e f r)
                                    (FaceData (Dart s) f)
faceDataOf fi = lens getF setF
  where
    (FaceId (VertexId i)) = fi
    getF ps = let (Raw ci wfi x) = ps^.rawFaceData.ix' i
                  fd             = ps^.component ci.dataOf wfi
              in fd&fData .~ x

    setF ps fd = let (Raw ci wfi _) = ps^.rawFaceData.ix' i
                     fd'            = fd&fData .~ fi
                     x              = fd^.fData
                 in ps&component ci.dataOf wfi   .~ fd'
                      &rawFaceData.ix' i.dataVal .~ x

instance HasDataOf (PlanarSubdivision s v e f r) (VertexId' s) where
  type DataOf (PlanarSubdivision s v e f r) (VertexId' s) = v
  dataOf v = vertexDataOf v.vData

instance HasDataOf (PlanarSubdivision s v e f r) (Dart s) where
  type DataOf (PlanarSubdivision s v e f r) (Dart s) = e
  dataOf d = rawDartData.ix' (fromEnum d).dataVal

instance HasDataOf (PlanarSubdivision s v e f r) (FaceId' s) where
  type DataOf (PlanarSubdivision s v e f r) (FaceId' s) = f
  dataOf f = faceDataOf f.fData



-- | Getter for the data at the endpoints of a dart
--
-- running time: \(O(1)\)
endPointsOf   :: Dart s -> Getter (PlanarSubdivision s v e f r )
                                  (VertexData r v, VertexData r v)
endPointsOf d = to (endPointData d)

-- | data corresponding to the endpoints of the dart
--
-- running time: \(O(1)\)
endPointData      :: Dart s -> PlanarSubdivision s v e f r
                  ->  (VertexData r v, VertexData r v)
endPointData d ps = let (u,v) = endPoints d ps
                    in (ps^.vertexDataOf u, ps^.vertexDataOf v)


--------------------------------------------------------------------------------

-- | gets the id of the outer face
--
-- running time: \(O(1)\)
outerFaceId :: PlanarSubdivision s v e f r -> FaceId' s
outerFaceId = const . FaceId . VertexId $ 0
  -- our invariant tells us the outerface is always at faceId 0

--------------------------------------------------------------------------------

-- | Reports all edges as line segments
edgeSegments    :: PlanarSubdivision s v e f r -> V.Vector (Dart s, LineSegment 2 v r :+ e)
edgeSegments ps = (\d -> (d,edgeSegment d ps)) <$> edges' ps


-- | Given a dart and the subdivision constructs the line segment representing it
--
-- \(O(1)\)
edgeSegment      :: Dart s -> PlanarSubdivision s v e f r -> LineSegment 2 v r :+ e
edgeSegment d ps = let (p,q) = bimap PG.vtxDataToExt PG.vtxDataToExt $ ps^.endPointsOf d
                   in ClosedLineSegment p q :+ ps^.dataOf d


-- | Generates the darts incident to a face, starting with the given dart.
--
--
-- \(O(k)\), where \(k\) is the number of darts reported
boundary'     :: Dart s -> PlanarSubdivision s v e f r -> V.Vector (Dart s)
boundary' d ps = let (_,d',g) = asLocalD d ps
                 in (\d'' -> g^.dataOf d'') <$> PG.boundary' d' g


-- | Constructs the outer boundary of the face
--
-- \(O(k)\), where \(k\) is the complexity of the outer boundary of the face
rawFaceBoundary      :: FaceId' s -> PlanarSubdivision s v e f r -> SimplePolygon v r :+ f
rawFaceBoundary i ps = fromPoints pts :+ (ps^.dataOf i)
  where
    d   = V.head $ outerBoundaryDarts i ps
    pts = (\d' -> PG.vtxDataToExt $ ps^.vertexDataOf (headOf d' ps))
       <$> V.toList (boundary' d ps)


-- | Constructs the boundary of the given face
--
-- \(O(k)\), where \(k\) is the complexity of the face
rawFacePolygon :: FaceId' s -> PlanarSubdivision s v e f r
                    -> SomePolygon v r :+ f
rawFacePolygon i ps = case F.toList $ holesOf i ps of
                        [] -> Left  res                               :+ x
                        hs -> Right (MultiPolygon vs $ map toHole hs) :+ x
  where
    res@(SimplePolygon vs) :+ x = rawFaceBoundary i ps
    toHole d = (rawFaceBoundary (leftFace d ps) ps)^.core

-- | Lists all faces of the planar subdivision.
rawFacePolygons    :: PlanarSubdivision s v e f r
                   -> V.Vector (FaceId' s, SomePolygon v r :+ f)
rawFacePolygons ps = fmap (\i -> (i,rawFacePolygon i ps)) . faces' $ ps



dartMapping ps = ps^.component (ComponentId 0).PG.dartData
