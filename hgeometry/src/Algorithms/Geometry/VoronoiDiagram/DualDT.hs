{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.VoronoiDiagram.DualDT where


import Control.Lens
import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Ball
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph
import           Data.PlaneGraph
import           Data.Proxy
import qualified Data.Vector as V

--------------------------------------------------------------------------------




type VoronoiDiagram s r p = PlaneGraph s Primal_ () () (Point 2 r :+ p) r






voronoiDiagram        :: (Ord r, Fractional r)
                      => proxy s -> NonEmpty.NonEmpty (Point 2 r :+ p)
                      -> PlanarGraph s Dual_ (Maybe (Point 2 r :+ ()))
                                             ()
                                             (Point 2 r :+ p)
                         --VoronoiDiagram s r p
voronoiDiagram px pts = undefined
  where
    -- dt' :: PlaneGraph s Primal_ p () () r
    dt' = toPlaneGraph px $ delaunayTriangulation pts
    dt  = dt'&faceData .~ (fmap (toVDVertex dt') . faces' $ dt')
    vd  = dual dt





toVDVertex        :: (Fractional r, Eq r)
                  => PlaneGraph s w v e f r -> FaceId s w
                  -> Maybe (Point 2 r :+ ())
toVDVertex dt fi
  | V.length bvs /= 3 = Nothing
  | otherwise         = disk a b c ^?_Just.center
    where
      bvs     = boundaryVertices fi dt
      [a,b,c] = map (\v -> dt^.dataOf v.core) . V.toList $ bvs
