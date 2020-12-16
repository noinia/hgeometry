--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon.Core
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- Implementation of Floyd-Warshall shortest path algorithm.
--
-- See Wikipedia article for details: https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
--
--------------------------------------------------------------------------------
module Algorithms.FloydWarshall
  ( mkIndex
  , mkGraph
  , floydWarshall
  ) where

import           Control.Monad               (forM_, when)
import           Control.Monad.ST            (ST, runST)
import           Data.Vector.Unboxed         (Vector, freeze)
import           Data.Vector.Unboxed.Mutable as V (MVector, length, replicate, unsafeRead,
                                                   unsafeWrite, Unbox)

-- | O(n^3)
floydWarshall :: (Unbox a, Num a, Ord a) => Int -> MVector s (a, Int) -> ST s ()
floydWarshall n graph = do
    let nSq = V.length graph
    when (n*n /= nSq) $ error "Bad bounds"
    forM_ [0 .. n-1] $ \k ->
      forM_ [0 .. n-1] $ \i ->
        forM_ [0 .. n-1] $ \j -> do
          (distIJ, _) <- access (i,j)
          (distIK, pathIK) <- access (i,k)
          (distKJ, _) <- access (k,j)
          let indirectDist = distIK + distKJ
          when (distIJ > indirectDist && distIJ > distIK && distIJ > distKJ) $
            put (i,j) (indirectDist, pathIK)
  where
    access idx = V.unsafeRead graph (mkIndex n idx)
    put idx e = V.unsafeWrite graph (mkIndex n idx) e

mkIndex :: Num a => a -> (a, a) -> a
mkIndex n (i,j) = i*n+j

mkGraph :: (Unbox a, Num a) => Int -> a -> [(Int,Int,a)] -> ST s (MVector s (a, Int))
mkGraph n maxValue edges = do
  graph <- V.replicate (n*n) (maxValue, maxBound)
  forM_ [0..n-1] $ \v -> do
    unsafeWrite graph (mkIndex n (v,v)) (0, v)
  forM_ edges $ \(i,j,cost) -> do
    unsafeWrite graph (mkIndex n (i,j)) (cost, j)
    unsafeWrite graph (mkIndex n (j,i)) (cost, i)
  return graph

-- test :: (UArray (Int, Int) Word, UArray (Int, Int) Int)
test :: Vector (Word, Int)
test = runST $ do
  let n = Prelude.length vertices
      vertices = [0,1,2]
      edges = [ (0,1,1), (1,2,1) ]
  graph <- mkGraph n maxBound edges
  floydWarshall n graph
  freeze graph
