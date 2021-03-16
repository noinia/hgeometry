module Algorithms.Geometry.UniformPolygonSampling where

import           Algorithms.Geometry.PolygonTriangulation.Triangulate
import           Control.Lens
import           Control.Monad.Random
import           Data.Ext
import           Data.Geometry.PlanarSubdivision (PolygonFaceData(..))
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core                           as Polygon
import           Data.Geometry.Triangle                               as Triangle
import qualified Data.List.NonEmpty                                   as NonEmpty
import           Data.PlaneGraph
import           Data.Proxy
import qualified Data.Vector                                          as V
import           Linear.Affine                                        hiding (Point)
import           Linear.Vector

-- | Uniformly samples a polygon in \(O(n \log n)\).
samplePolygon :: (RandomGen g, Random r, Fractional r, Ord r, Real r) => Polygon t p r -> Rand g (Point 2 r)
samplePolygon p = do
   randTri <- fromList $ map (\tri -> (tri, toRational $ areaRatio tri)) $ toTriangles p
   sampleTriangle randTri
   where
      areaRatio tri = Triangle.area tri / Polygon.area p

-- | Uniformly samples a triangle in \(O(1)\).
sampleTriangle :: (RandomGen g, Random r, Fractional r, Ord r) => Triangle 2 p r -> Rand g (Point 2 r)
sampleTriangle (Triangle v1 v2 v3) = do
  a' <- getRandomR (0, 1)
  b' <- getRandomR (0, 1)
  let (a, b) = if a' + b' > 1 then (1 - a', 1 - b') else (a', b')
  return $ v1^.core .+^ a*^u .+^ b*^v
  where
    u = v2^.core .-. v1^.core
    v = v3^.core .-. v1^.core

-- | Triangulates a polygon \(O(n \log n)\).
toTriangles :: (Fractional r, Ord r) => Polygon t p r -> [Triangle 2 p r]
toTriangles p =
    [ (polygonToTriangle . view core . (`rawFacePolygon` g)) f
    | (f, Inside) <- V.toList (faces g) ]
  where
    g = triangulate' Proxy p
    polygonToTriangle poly = case NonEmpty.toList $ polygonVertices poly of
      [v1, v2, v3] -> Triangle v1 v2 v3
      _            -> error "Invalid Triangulation"
