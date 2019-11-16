{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.VoronoiDiagram.DualDT where


import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.HalfLine
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph(FaceId)
import           Data.PlaneGraph(PlaneGraph)
import qualified Data.PlaneGraph as PG
import           Data.Proxy
import qualified Data.Vector as V

--------------------------------------------------------------------------------

type UnboundedVoronoiEdges s e r = V.Vector (VertexId' s, HalfLine 2 r :+ e)


data VertexType v = BoundingBoxVertex | VoronoiVertex !v deriving (Show,Eq,Ord)

data SiteData f r = SiteData !(Point 2 r) !f deriving (Show,Eq)


data VoronoiDiagram s v e f r = VoronoiDiagram {
      _boundedDiagram         :: !(PlanarSubdivision s (VertexType v) e (SiteData f r) r)
    , _boundedArea            :: !(Rectangle () r)
    , _unboundedIntersections :: !(UnboundedVoronoiEdges s e r)
    } deriving (Show,Eq)








voronoiDiagram        :: (Ord r, Fractional r)
                      => proxy s -> NonEmpty.NonEmpty (Point 2 r :+ p)
                      -> VoronoiDiagram s () () p r
voronoiDiagram px pts = VoronoiDiagram diag bBox unb'
  where
    oid = PG.outerFaceId dt

    -- dt' :: PlaneGraph s Primal_ p () () r
    dt' = toPlaneGraph px $ delaunayTriangulation pts
    dt  = dt'&PG.faceData .~ (fmap (toVDVertex dt') . PG.faces' $ dt')

    bBox = grow 1 . boundingBoxList' . V.mapMaybe snd . PG.faces $ dt

    diag = undefined

    unb = unBoundedEdge dt <$> PG.boundary oid dt
         -- this gives the unboundededges from their actual voronoi vertices
    unb' = undefined


-- | Computes the unbounded edge corresponding to this dart of the
-- convex hull.
--
-- running time: \(O(1)\)
unBoundedEdge      :: Fractional r
                   => PlaneGraph s v e (Maybe (Point 2 r)) r -- ^ the delaunaytriangulation
                   -> Dart s
                   -> (FaceId' s, HalfLine 2 r :+ ())
unBoundedEdge dt d = let (p,q)  = over both (^.location) $ dt^.PG.endPointsOf d
                         fi     = PG.leftFace d dt
                         Just v = dt^.dataOf fi
                           -- the face to the left of this dart should be a triangle
                           -- and thus have a
                     in (fi, ext $ unboundedEdgeFrom v p q)


-- | Given an edge of the convex hull (specifiekd by two adjacent
-- vertices) and the vertex in the Voronoi diagram that is incident to
-- these two sites, computes the halfine representing their unbouded
-- edge of the VD.
unboundedEdgeFrom       :: Fractional r
                        => Point 2 r -- ^ The starting point of the unbounded edge
                        -> Point 2 r -- ^ vertex of the CH
                        -> Point 2 r -- ^ adjacent vertex of the CH
                        -> HalfLine 2 r
unboundedEdgeFrom v p q = HalfLine v (midPoint p q .-. v)
  where
    midPoint a b = a .+^ ((b^.vector) ^/ 2)

-- | Computes the location of a Voronoi vertex
--
toVDVertex        :: (Fractional r, Eq r)
                  => PlaneGraph s v e f r -> FaceId' s
                  -> Maybe (Point 2 r)
toVDVertex dt fi
  | V.length bvs /= 3 = Nothing
  | otherwise         = disk a b c ^?_Just.center.core
    where
      bvs     = PG.boundaryVertices fi dt
      [a,b,c] = map (\v -> dt^.PG.locationOf v) . V.toList $ bvs
