module Main where

import qualified Algorithms.Geometry.ClosestPair.Bench as CP
-- import qualified Algorithms.Geometry.LineSegmentIntersection.Bench as M
-- import qualified Algorithms.Geometry.PolygonTriangulation.Bench as M
import qualified Algorithms.Geometry.ConvexHull.Bench as M
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain [ CP.benchmark, M.benchmark ]
