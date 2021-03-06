module Algorithms.Geometry.UniformPolygonSampling where

import Control.Monad.Random
import Data.Geometry.Point
import Data.Geometry.Polygon.Core
import Data.Geometry.Triangle
import Control.Lens
import Data.Ext
import Linear.Affine hiding (Point)
import Linear.Vector

-- | O(n log n)
samplePolygon :: (RandomGen g, Random r, Fractional r) => Polygon t p r -> Rand g (Point 2 r)
samplePolygon = error "not implemented yet"

-- | O(1)
sampleTriangle :: (RandomGen g, Random r, Fractional r, Ord r) => Triangle 2 p r -> Rand g (Point 2 r)
sampleTriangle (Triangle v1 v2 v3) = do
  a' <- getRandomR (0, 1)
  b' <- getRandomR (0, 1)
  let (a, b) = if a' + b' > 1 then (1 - a', 1 - b') else (a', b') in
    return $ v1^.core .+^ a*^u .+^ b*^v
  where
    u = v2^.core .-. v1^.core
    v = v3^.core .-. v1^.core

-- | O(n log n)
toTriangles :: (Fractional r, Ord r) => Polygon t p r -> [Triangle 2 p r]
toTriangles = error "not implemented yet"