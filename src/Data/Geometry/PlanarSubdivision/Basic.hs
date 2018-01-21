{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.PlanarSubdivision.Basic where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Data.Aeson
import           Data.Coerce
import           Data.Permutation(ix')
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph (toAdjacencyLists,buildFromJSON, isPositive)
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


--------------------------------------------------------------------------------

data Raw s ia a = Raw { _compId  :: {-# UNPACK #-} !(ComponentId s)
                      , _idxVal  :: {-# UNPACK #-} !ia
                      , _dataVal :: !a
                      } deriving (Eq,Show,Functor,Foldable,Traversable)
makeLenses ''Raw



-- | A planarsubdivision is essentially a bunch of plane-graphs; one for every
-- connected component. These graphs store the global ID's (darts, vertexId's, faceId's)
-- in their data values. This essentially gives us a mapping between the two.
--
-- note that a face may actually occur in multiple graphs, hence when we store
-- the edges to the the holes, we store the global edgeId's rather than the
-- 'local' edgeId (dart)'s.
data PlanarSubdivision s v e f r =
  PlanarSubdivision { _graphs      :: V.Vector (PlaneGraph (Wrap s)
                                                           (VertexId' s)
                                                           (Dart s)
                                                           (FaceData (Dart s) (FaceId' s))
                                                           r)
                    , _rawVertexData :: V.Vector (Raw s (VertexId' (Wrap s)) v)
                    , _rawDartData   :: V.Vector (Raw s (Dart      (Wrap s)) e)
                    , _rawFaceData   :: V.Vector (Raw s (FaceId'   (Wrap s)) f)
                    } deriving (Show,Eq,Functor)
makeLenses ''PlanarSubdivision


-- TODO: faceId 0 will be the outer face






type instance NumType   (PlanarSubdivision s v e f r) = r
type instance Dimension (PlanarSubdivision s v e f r) = 2

instance IsBoxable (PlanarSubdivision s v e f r) where
  boundingBox = boundingBoxList' . V.toList . _graphs


component    :: ComponentId s -> Lens' (PlanarSubdivision s v e f r)
                                       (PlaneGraph (Wrap s)
                                                   (VertexId' s)
                                                   (Dart s)
                                                   (FaceData (Dart s) (FaceId' s))
                                                   r)
component ci = graphs.ix' (unCI ci)







--------------------------------------------------------------------------------

fromPlaneGraph   :: forall s v e f r. PlaneGraph s v e f r -> PlanarSubdivision s v e f r
fromPlaneGraph g = PlanarSubdivision (V.singleton . coerce $ g') vd ed fd
  where
    c = ComponentId 0
    vd = V.imap (\i v  -> Raw c (VertexId i) v)          $ g^.PG.vertexData
    ed = fmap (\(d,dd) -> Raw c (coerce d)   dd)         $ g^.PG.dartData
    fd = V.imap (\i f  -> Raw c (FaceId (VertexId i)) f) $ g^.PG.faceData

    g' :: PlaneGraph s (VertexId' s) (Dart s) (FaceData (Dart s) (FaceId' s)) r
    g' = g&PG.faceData   %~ V.imap (\i _ -> FaceData mempty (FaceId (VertexId i)))
          &PG.vertexData %~ V.imap (\i _ -> VertexId i)
          &PG.dartData   %~ fmap (\(d,_) -> (d,d))

-- rawDartData


-- | Construct a planar subdivision from a simple polygon
--
-- running time: \(O(n)\).
fromSimplePolygon            :: proxy s
                             -> SimplePolygon p r
                             -> f -- ^ data inside
                             -> f -- ^ data outside the polygon
                             -> PlanarSubdivision s p () f r
fromSimplePolygon p pg iD oD = fromPlaneGraph $ PG.fromSimplePolygon p pg iD oD

-- | Constructs a connected planar subdivision.
--
-- pre: the segments form a single connected component
-- running time: \(O(n\log n)\)
fromConnectedSegments    :: (Foldable f, Ord r, Num r)
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
vertices' = fmap fst . vertices

-- | Enumerate all vertices, together with their vertex data

-- >>> vertices myGraph
-- [(VertexId 0,()),(VertexId 1,()),(VertexId 2,()),(VertexId 3,())]
vertices   :: PlanarSubdivision s v e f r  -> V.Vector (VertexId' s, VertexData r v)
vertices = undefined
  -- V.imap (\i (_,_,v) -> (VertexId i, v)) . _vertexData

-- | Enumerate all darts
darts' :: PlanarSubdivision s v e f r  -> V.Vector (Dart s)
darts' = undefined

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
edges :: PlanarSubdivision s v e f r  -> V.Vector (Dart s, e)
edges = undefined -- PG.edges . _planeGraph







--------------------------------------------------------------------------------
-- * Access data




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
                                    (FaceData (Dart s) (FaceId' s), f)
faceDataOf fi = lens getF setF
  where
    (FaceId (VertexId i)) = fi
    getF ps = let (Raw ci wfi x) = ps^.rawFaceData.ix' i
              in (ps^.component ci.dataOf wfi, x)

    setF ps (fd,x) = let (Raw ci wfi _) = ps^.rawFaceData.ix' i
                     in ps&component ci.dataOf wfi   .~ fd
                          &rawFaceData.ix' i.dataVal .~ x

instance HasDataOf (PlanarSubdivision s v e f r) (VertexId' s) where
  type DataOf (PlanarSubdivision s v e f r) (VertexId' s) = v
  dataOf v = vertexDataOf v.vData

instance HasDataOf (PlanarSubdivision s v e f r) (Dart s) where
  type DataOf (PlanarSubdivision s v e f r) (Dart s) = e
  dataOf d = rawDartData.ix' (fromEnum d).dataVal

instance HasDataOf (PlanarSubdivision s v e f r) (FaceId' s) where
  type DataOf (PlanarSubdivision s v e f r) (FaceId' s) = f
  dataOf f = faceDataOf f._2







-- -- | Getter for the data at the endpoints of a dart
-- --
-- -- running time: \(O(1)\)
-- endPointsOf   :: Dart s -> Getter (PlanarSubdivision s v e f r )
--                                   (VertexData r v, VertexData r v)
-- endPointsOf d = planeGraph.PG.endPointsOf d

-- -- | Data corresponding to the endpoints of the dart
-- --
-- -- running time: \(O(1)\)
-- endPointData   :: Dart s -> PlanarSubdivision s v e f r
--                ->  (VertexData r v, VertexData r v)
-- endPointData d = PG.endPointData d . _planeGraph
