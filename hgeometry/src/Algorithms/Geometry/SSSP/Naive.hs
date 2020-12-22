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
  , sssp'
  ) where

import           Algorithms.FloydWarshall  (floydWarshall, mkGraph, mkIndex)
import           Algorithms.Geometry.SSSP  (SSSP)
import           Control.Lens              ((^.))
import           Control.Monad.ST          (runST)
import           Data.Ext                  (core)
import qualified Data.Foldable             as F
import           Data.Geometry.Interval    (EndPoint (Closed, Open), end, start)
import           Data.Geometry.LineSegment (LineSegment (..), sqSegmentLength)
import           Data.Geometry.Polygon     (SimplePolygon, listEdges, outerBoundary)
import           Data.Intersection         (IsIntersectableWith (intersect),
                                            NoIntersection (NoIntersection))
import           Data.List                 (tails)
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import           Data.Vinyl                (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec          (Handler (H), match)

-- | O(n^3) Single-Source Shortest Path.
sssp :: (Real r, Fractional r) => SimplePolygon p r -> SSSP
sssp = V.head . sssp'

-- | O(n^3) Single-Source Shortest Path from all vertices.
sssp' :: (Real r, Fractional r) => SimplePolygon p r -> Vector SSSP
sssp' p = runST $ do
    -- Create an n*n matrix containing paths and distances between vertices.
    graph <- mkGraph n infinity (visibleEdges p)
    -- Use FloydWarshall O(n^3) to complete the matrix.
    floydWarshall n graph
    -- Create a tree describing the shortest path from any node to the 0th node.
    g <- VU.unsafeFreeze graph
    pure $ V.generate n $ \origin ->
      VU.generate n $ \i ->
        let (_dist, next) = g VU.! mkIndex n (i, origin)
        in next
  where
    infinity = read "Infinity" :: Double
    n = F.length (p ^. outerBoundary)

-- O(n^3)
visibleEdges :: (Real r, Fractional r) => SimplePolygon p r -> [(Int, Int, Double)]
visibleEdges p = concat
  [
    [ (i, j, sqrt (realToFrac (sqSegmentLength line)))
    | (j, endPt) <- rest
    , let line = LineSegment (Closed pt) (Open endPt)
    , not (interiorIntersection line edges)
    ]
  | (i, pt) <- pts
  | rest <- Prelude.drop 1 $ tails pts
  ]
  where
    pts = Prelude.zip [0..] (F.toList (p ^. outerBoundary))
    edges = listEdges p

interiorIntersection :: (Ord r, Fractional r) => LineSegment 2 p r -> [LineSegment 2 p r] -> Bool
interiorIntersection _ [] = False
interiorIntersection l (x:xs) =
  match (l `intersect` x) (
       H (\NoIntersection -> False)
    :& H (\pt -> pt /= l^.start.core && pt /= l^.end.core)
    :& H (\_line -> True)
    :& RNil)
  || interiorIntersection l xs
