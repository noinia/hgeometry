{-# LANGUAGE ParallelListComp #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SSSP.Naive
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--------------------------------------------------------------------------------
module Algorithms.Geometry.SSSP.Naive
  ( sssp
  ) where

import           Algorithms.FloydWarshall                    (floydWarshall, mkGraph, mkIndex)
import           Algorithms.Geometry.LineSegmentIntersection (hasInteriorIntersections)
import           Algorithms.Geometry.SSSP                    (SSSP)
import           Control.Lens                                ((^.))
import           Control.Monad.ST                            (runST)
import qualified Data.Foldable                               as F
import           Data.Geometry.LineSegment                   (EndPoint (Closed, Open), pattern LineSegment,
                                                              sqSegmentLength)
import           Data.Geometry.Polygon                       (SimplePolygon, listEdges,
                                                              outerBoundary)
import           Data.List                                   (tails)
import qualified Data.Vector.Unboxed                         as VU

-- | O(n^3 log n) Single-Source Shortest Path.
sssp :: (Real r, Fractional r) => SimplePolygon p r -> SSSP
sssp p = runST $ do
    -- Create an n*n matrix containing paths and distances between vertices.
    graph <- mkGraph n infinity (visibleEdges p)
    -- Use FloydWarshall O(n^3) to complete the matrix.
    floydWarshall n graph
    -- Create a tree describing the shortest path from any node to the 0th node.
    g <- VU.unsafeFreeze graph
    pure $ VU.generate n $ \i ->
      let (_dist, next) = g VU.! (mkIndex n (i, 0))
      in next
  where
    infinity = read "Infinity" :: Double
    n = F.length (p ^. outerBoundary)

-- O(n^3 log n)
visibleEdges :: (Real r, Fractional r) => SimplePolygon p r -> [(Int, Int, Double)]
visibleEdges p = concat
  [
    [ (i, j, sqrt (realToFrac (sqSegmentLength line)))
    | (j, endPt) <- rest
    , let line = LineSegment (Closed pt) (Open endPt)
    , not (hasInteriorIntersections (line : edges))
    ]
  | (i, pt) <- pts
  | rest <- Prelude.drop 1 $ tails pts
  ]
  where
    pts = Prelude.zip [0..] (F.toList (p ^. outerBoundary))
    edges = listEdges p
