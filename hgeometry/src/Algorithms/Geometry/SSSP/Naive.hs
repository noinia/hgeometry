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
import           Control.Lens
import           Control.Monad.ST          (runST)
import           Data.Ext                  (_core, core)
import qualified Data.Foldable             as F
import           Geometry.Interval    (EndPoint (Closed, Open), end, start)
import           Geometry.LineSegment (LineSegment (..), sqSegmentLength)
import           Geometry.Point.WithExtra       (ccwCmpAroundWith')
import           Geometry.Polygon     (SimplePolygon, listEdges, outerBoundaryVector)
import           Data.Intersection         (IsIntersectableWith (intersect),
                                            NoIntersection (NoIntersection))
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Unboxed       as VU
import           Data.Vinyl                (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec          (Handler (H), match)
import           Linear.Affine             ((.-.))

type SSSP = VU.Vector Int

-- | \( O(n^3) \) Single-Source Shortest Path.
sssp :: (Real r, Fractional r) => SimplePolygon p r -> SSSP
sssp p = V.head . sssp' $ p

-- | \( O(n^3) \) Single-Source Shortest Path from all vertices.
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
    n = F.length (p ^. outerBoundaryVector)

-- \( O(n^3) \)
visibleEdges :: (Real r, Fractional r) => SimplePolygon p r -> [(Int, Int, Double)]
visibleEdges p = concat
  [
    [ (i, j, sqrt (realToFrac (sqSegmentLength line)))
    | j <- [i+2 .. n-1]
    , let endPt = CV.index vs j
    , let line = LineSegment (Closed pt) (Open endPt)
      -- Check if the line goes through the inside of the polygon.
    , ccwCmpAroundWith' ((_core prev) .-. (_core pt)) pt endPt next == GT
      -- Check if there are any intersections not the line end points.
    , not (interiorIntersection line edges)
    ]
  | i <- [0 .. n-1]
  , let pt = CV.index vs i
        prev = CV.index vs (i-1)
        next = CV.index vs (i+1)
  ] ++
  [ (i,(i+1)`mod`n,sqrt (realToFrac (sqSegmentLength edge)))
  | (i, edge) <- zip [0..] edges
  ]
  where
    vs = p^.outerBoundaryVector
    n = F.length vs
    edges = listEdges p

interiorIntersection :: (Ord r, Fractional r) => LineSegment 2 p r -> [LineSegment 2 p r] -> Bool
interiorIntersection _ [] = False
interiorIntersection l (x:xs) =
  match (l `intersect` x) (
       H (\NoIntersection -> False)
    :& H (\pt -> pt /= l^.start.core && pt /= l^.end.core)
    :& H (\line -> sqSegmentLength line /= 0)
    :& RNil)
  || interiorIntersection l xs
