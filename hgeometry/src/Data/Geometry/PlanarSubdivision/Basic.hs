{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlnarSubdivision.Basic
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Basic data types to represent a PlanarSubdivision
--
--------------------------------------------------------------------------------
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

                                            , numComponents, numVertices
                                            , numEdges, numFaces, numDarts
                                            , dual

                                            , components, component
                                            , vertices', vertices
                                            , edges', edges
                                            , faces', faces, internalFaces
                                            , darts'
                                            -- , traverseVertices, traverseDarts, traverseFaces

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

                                            , faceDataOf

                                            , edgeSegment, edgeSegments
                                            , rawFacePolygon, rawFaceBoundary
                                            , rawFacePolygons

                                            , VertexId(..), FaceId(..), Dart, World(..)

                                            , rawVertexData, rawDartData, rawFaceData
                                            , vertexData, dartData, faceData
                                            , dataVal

                                            , dartMapping, Raw(..)
                                            ) where

import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.LineSegment hiding (endPoints)
import           Data.Geometry.PlanarSubdivision.Raw
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph.Dart (allDarts,isPositive)
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A connected component.
--
-- For every face f, and every hole in this face, the facedata points to a dart
-- d on the hole s.t. this dart has the face f on its left. i.e.
-- leftFace d = f
type Component s r = PlaneGraph (Wrap s)
                                (VertexId' s) (Dart s) (FaceId' s)
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
                    , _rawFaceData   :: V.Vector (RawFace s f)
                    } deriving (Show,Eq,Functor,Generic)
makeLenses ''PlanarSubdivision


type instance NumType   (PlanarSubdivision s v e f r) = r
type instance Dimension (PlanarSubdivision s v e f r) = 2

instance IsBoxable (PlanarSubdivision s v e f r) where
  boundingBox = boundingBoxList' . V.toList . _components


component    :: ComponentId s -> Lens' (PlanarSubdivision s v e f r)
                                       (Component s r)
component ci = components.singular (ix $ unCI ci)

-- instance (ToJSON v, ToJSON v, ToJSON e, ToJSON f, ToJSON r)
--          => ToJSON (PlanarSubdivision s v e f r) where
--   toEncoding = genericToEncoding defaultOptions



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
    vd = V.imap    (\i v   -> Raw c (VertexId i) v)                   $ g^.PG.vertexData
    ed = V.zipWith (\d dd  -> Raw c d dd) allDarts''                  $ g^.PG.rawDartData
    fd = V.imap (\i f      -> RawFace (mkFaceIdx i) (mkFaceData i f)) $ g^.PG.faceData

    g' :: PlaneGraph s (VertexId' s) (Dart s) (FaceId' s) r
    g' = g&PG.faceData    %~ V.imap (\i _ -> mkFaceId $ flipID i)
          &PG.vertexData  %~ V.imap (\i _ -> VertexId i)
          &PG.rawDartData .~ allDarts''

    allDarts'' :: forall s'. V.Vector (Dart s')
    allDarts'' = allDarts' (PG.numDarts g)

    -- make sure the outerFaceId is 0
    oF@(FaceId (VertexId of')) = PG.leftFace ofD g

    mkFaceIdx i | i == 0    = Nothing
                | otherwise = Just (c,mkFaceId . flipID $ i)

    -- at index i we are storing the outerface
    mkFaceData                 :: Int -> f -> FaceData (Dart s) f
    mkFaceData i f | i == 0    = FaceData (Seq.singleton ofD) (g^.dataOf oF)
                   | i == of'  = FaceData mempty              (g^.dataOf (mkFaceId @s 0))
                   | otherwise = FaceData mempty              f

    mkFaceId :: forall s'. Int -> FaceId' s'
    mkFaceId = FaceId . VertexId

    flipID i | i == 0    = of'
             | i == of'  = 0
             | otherwise = i

-- | Construct a plane graph from a simple polygon. It is assumed that the
-- polygon is given in counterclockwise order.
--
-- the interior of the polygon will have faceId 0
--
-- pre: the input polygon is given in counterclockwise order
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

-- g1 = PG.fromConnectedSegments (Identity Test1) testSegs
-- ps1 = fromConnectedSegments (Identity Test1) testSegs

-- data Test1 = Test1

-- draw = V.filter isEmpty . rawFacePolygons
--   where
--     isEmpty (_,Left  p :+ _) = (< 3) . length . polygonVertices $ p
--     isEmpty (_,Right p :+ _) = (< 3) . length . polygonVertices $ p

-- testSegs = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
--                    [ (origin, Point2 10 10)
--                    , (origin, Point2 12 10)
--                    , (origin, Point2 20 5)
--                    , (origin, Point2 13 20)
--                    , (Point2 10 10, Point2 12 10)
--                    , (Point2 10 10, Point2 13 20)
--                    , (Point2 12 10, Point2 20 5)
--                    ]


--------------------------------------------------------------------------------

-- | Data type that expresses whether or not we are inside or outside the
-- polygon.
data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)


--------------------------------------------------------------------------------
-- * Basic Graph information

-- | Get the number of vertices
--
-- >>> numVertices myGraph
-- 1
numComponents :: PlanarSubdivision s v e f r  -> Int
numComponents = V.length . _components

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
internalFaces    :: PlanarSubdivision s v e f r
                 -> V.Vector (FaceId' s, FaceData (Dart s) f)
internalFaces ps = V.tail $ faces ps
  -- this uses that the outerfaceId is 0, and thus it is the first face in the vector.

-- | lens to access the Dart Data
dartData :: Lens (PlanarSubdivision s v e f r) (PlanarSubdivision s v e' f r)
                 (V.Vector (Dart s, e))        (V.Vector (Dart s, e'))
dartData = lens getF setF
  where
    getF     = V.imap (\i x -> (toEnum i, x^.dataVal)) . _rawDartData
    setF ps ds' = ps&rawDartData %~ mkDS' ds'

    -- create a new dartData vector to assign the values to
    mkDS' ds' ds = V.create $ do
                     v <- MV.new (V.length ds)
                     mapM_ (assignDart ds v) ds'
                     pure v

    assignDart ds v (d,x) = let i = fromEnum d
                                y = ds V.! i
                            in MV.write v i (y&dataVal .~ x)


-- | Lens to the facedata of the faces themselves. The indices correspond to the faceIds
faceData :: Lens (PlanarSubdivision s v e f r) (PlanarSubdivision s v e f' r)
                 (V.Vector f)                  (V.Vector f')
faceData = lens getF setF
  where
    getF = fmap (^.faceDataVal.fData) . _rawFaceData
    setF ps v' = ps&rawFaceData %~ V.zipWith (\x' x -> x&faceDataVal.fData .~ x') v'

-- | Lens to the facedata of the vertexdata themselves. The indices correspond to the vertexId's
vertexData :: Lens (PlanarSubdivision s v e f r) (PlanarSubdivision s v' e f r)
                   (V.Vector v)                  (V.Vector v')
vertexData = lens getF setF
  where
    getF = fmap (^.dataVal) . _rawVertexData
    setF ps v' = ps&rawVertexData %~ V.zipWith (\x' x -> x&dataVal .~ x') v'


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

-- | All edges incident to vertex v in incoming direction
-- (i.e. pointing into v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
incomingEdges      :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (Dart s)
incomingEdges v ps = orient <$> incidentEdges v ps
  where
    orient d = if headOf d ps == v then d else twin d

-- | All edges incident to vertex v in outgoing direction
-- (i.e. pointing away from v) in counterclockwise order around v.
--
-- running time: \(O(k)\), where \(k) is the total number of incident edges of v
outgoingEdges      :: VertexId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
outgoingEdges v ps = orient <$> incidentEdges v ps
  where
    orient d = if tailOf d ps == v then d else twin d


-- | Gets the neighbours of a particular vertex, in counterclockwise order
-- around the vertex.
--
-- running time: \(O(k)\), where \(k\) is the output size
neighboursOf      :: VertexId' s -> PlanarSubdivision s v e f r -> V.Vector (VertexId' s)
neighboursOf v ps = flip tailOf ps <$> incomingEdges v ps

-- | The face to the left of the dart
--
-- running time: \(O(1)\).
leftFace      :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
leftFace d ps = let (_,d',g) = asLocalD d ps
                    fi       = PG.leftFace d' g
                in g^.dataOf fi

-- | The face to the right of the dart
--
-- running time: \(O(1)\).
rightFace      :: Dart s -> PlanarSubdivision s v e f r  -> FaceId' s
rightFace d ps = let (_,d',g) = asLocalD d ps
                     fi       = PG.rightFace d' g
                in g^.dataOf fi

-- | The darts on the outer boundary of the face, for internal faces
-- the darts are in clockwise order. For the outer face the darts are
-- in counterclockwise order, and the darts from various components are in no particular order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
outerBoundaryDarts      :: FaceId' s -> PlanarSubdivision s v e f r  -> V.Vector (Dart s)
outerBoundaryDarts f ps = V.concatMap single . V.fromList . NonEmpty.toList $ asLocalF f ps
  where
    single (_,f',g) = (\d -> g^.dataOf d) <$> PG.boundary f' g

-- | Get the local face and component from a given face.
asLocalF                          :: FaceId' s -> PlanarSubdivision s v e f r
                                  -> NonEmpty (ComponentId s, FaceId' (Wrap s), Component s r)
asLocalF (FaceId (VertexId f)) ps = case ps^?!rawFaceData.ix f of
      RawFace (Just (ci,f')) _        -> (ci,f',ps^.component ci) :| []
      RawFace Nothing (FaceData hs _) -> toLocalF <$> NonEmpty.fromList (F.toList hs)
  where
    toLocalF d = let (ci,d',c) = asLocalD d ps in (ci,PG.leftFace d' c,c)

-- | The vertices of the outer boundary of the face, for internal faces in
-- clockwise order, for the outer face in counter clockwise order.
--
--
-- running time: \(O(k)\), where \(k\) is the output size.
boundaryVertices      :: FaceId' s -> PlanarSubdivision s v e f r
                      -> V.Vector (VertexId' s)
boundaryVertices f ps = (\d -> headOf d ps) <$> outerBoundaryDarts f ps


-- | Lists the holes in this face, given as a list of darts to arbitrary darts
-- on those faces. The returned darts are on the outside of the hole, i.e. they are
-- incident to the given input face:
--
-- prop> all (\d -> leftFace d ps == fi) $ holesOf fi ps
--
-- running time: \(O(k)\), where \(k\) is the number of darts returned.
holesOf       :: FaceId' s -> PlanarSubdivision s v e f r -> Seq.Seq (Dart s)
holesOf fi ps = ps^.faceDataOf fi.holes


--------------------------------------------------------------------------------
-- * Access data



asLocalD      :: Dart s -> PlanarSubdivision s v e f r
              -> (ComponentId s, Dart (Wrap s), Component s r)
asLocalD d ps = let (Raw ci d' _) = ps^?!rawDartData.ix (fromEnum d)
                in (ci,d',ps^.component ci)




asLocalV                 :: VertexId' s -> PlanarSubdivision s v e f r
                         -> (ComponentId s, VertexId' (Wrap s), Component s r)
asLocalV (VertexId v) ps = let (Raw ci v' _) = ps^?!rawVertexData.ix v
                           in (ci,v',ps^.component ci)

-- | Note that using the setting part of this lens may be very expensive!!
vertexDataOf               :: VertexId' s
                           -> Lens' (PlanarSubdivision s v e f r ) (VertexData r v)
vertexDataOf (VertexId vi) = lens get' set''
  where
    get' ps = let (Raw ci wvdi x) = ps^?!rawVertexData.ix vi
                  vd              = ps^.component ci.PG.vertexDataOf wvdi
              in vd&vData .~ x
    set'' ps x = let (Raw ci wvdi _)  = ps^?!rawVertexData.ix vi
                 in ps&rawVertexData.ix vi.dataVal                .~ (x^.vData)
                      &component ci.PG.vertexDataOf wvdi.location .~ (x^.location)

locationOf   :: VertexId' s -> Lens' (PlanarSubdivision s v e f r ) (Point 2 r)
locationOf v = vertexDataOf v.location


faceDataOf    :: FaceId' s -> Lens' (PlanarSubdivision s v e f r)
                                    (FaceData (Dart s) f)
faceDataOf fi = lens getF setF
  where
    (FaceId (VertexId i)) = fi
    getF ps = ps^?!rawFaceData.ix i.faceDataVal
    setF ps fd = ps&rawFaceData.ix i.faceDataVal .~ fd

instance HasDataOf (PlanarSubdivision s v e f r) (VertexId' s) where
  type DataOf (PlanarSubdivision s v e f r) (VertexId' s) = v
  dataOf v = vertexDataOf v.vData

instance HasDataOf (PlanarSubdivision s v e f r) (Dart s) where
  type DataOf (PlanarSubdivision s v e f r) (Dart s) = e
  dataOf d = rawDartData.singular (ix (fromEnum d)).dataVal

instance HasDataOf (PlanarSubdivision s v e f r) (FaceId' s) where
  type DataOf (PlanarSubdivision s v e f r) (FaceId' s) = f
  dataOf f = faceDataOf f.fData


-- -- | Traverse the vertices
-- --
-- traverseVertices   :: Applicative m
--                    => (VertexId' s -> v -> m v')
--                    -> PlanarSubdivision s v e f r
--                    -> m (PlanarSubdivision s v' e f r)
-- traverseVertices f = itraverseOf (vertexData.itraversed) (\i -> f (VertexId i))

-- -- | Traverses the darts
-- --
-- traverseDarts   :: Applicative m
--                 => (Dart s -> e -> m e')
--                 -> PlanarSubdivision s v e f r
--                 -> m (PlaneGraph s v e' f r)
-- traverseDarts f = traverseOf (dart) (PG.traverseDarts f)


-- -- | Traverses the faces
-- --
-- traverseFaces   :: Applicative m
--                 => (FaceId' s  -> f -> m f')
--                 -> PlaneGraph s v e f r
--                 -> m (PlaneGraph s v e f' r)
-- traverseFaces f = traverseOf graph (PG.traverseFaces f)


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
rawFacePolygon      :: FaceId' s -> PlanarSubdivision s v e f r
                    -> SomePolygon v r :+ f
rawFacePolygon i ps = case F.toList $ holesOf i ps of
                        [] -> Left  res                               :+ x
                        hs -> Right (MultiPolygon vs $ map toHole hs) :+ x
  where
    res@(SimplePolygon vs) :+ x = rawFaceBoundary i ps
    toHole d = (rawFaceBoundary (leftFace d ps) ps)^.core

-- | Lists all *internal* faces of the planar subdivision.
rawFacePolygons    :: PlanarSubdivision s v e f r
                   -> V.Vector (FaceId' s, SomePolygon v r :+ f)
rawFacePolygons ps = fmap (\(i,_) -> (i,rawFacePolygon i ps)) . internalFaces $ ps



dartMapping    :: PlanarSubdivision s v e f r -> V.Vector (Dart (Wrap s), Dart s)
dartMapping ps = ps^.component (ComponentId 0).PG.dartData



--------------------------------------------------------------------------------

-- data Id a = Id a
-- data Test = Test

-- triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
-- triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
--          $ trianglePG

-- trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]
