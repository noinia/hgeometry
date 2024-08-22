--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PolyLine.Frechet.Discrete
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes the discrete frechet distance
--
--------------------------------------------------------------------------------
module HGeometry.PolyLine.Frechet.Discrete
  ( frechetDistanceWith
  ) where

import           Control.Lens
import           Data.Array
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Point
import           HGeometry.PolyLine.Class
import           Hiraffe.Graph

import Debug.Trace
--------------------------------------------------------------------------------

-- | Computes the discrete frechet distance with respect to the given distance function.
--
-- O(nm*(I+T)), where I is the time to index one of the vertices and T is the time to
-- evaluate the distance function.
frechetDistanceWith :: ( PolyLine_ polyLine  point
                       , PolyLine_ polyLine' point'
                       , Ord r
                       , VertexIx polyLine ~ Int, VertexIx polyLine' ~ Int
                       )
                    => (point -> point' -> r)
                    -> polyLine -> polyLine'
                    -> r
frechetDistanceWith dist' polyP polyQ = fd (n,m)
  where
    n = numVertices polyP - 1
    m = numVertices polyQ - 1
    dist i j = dist' (polyP^?!vertexAt i) (polyQ^?!vertexAt j)

    -- use dynamic programming
    fd = memo ((0, 0), (n, m)) fd'

    fd' (0,0) = dist 0 0
    fd' (0,q) = dist 0 q `max` fd (0, q-1)
    fd' (p,0) = dist p 0 `max` fd (p-1, 0)
    fd' (p,q) = dist p q `max` (fd (p, q-1) `min` fd (p-1, q) `min` fd (p-1, q-1))
    -- we either move on the q side, on the p side, or both

--------------------------------------------------------------------------------
-- * Dynamic programming / Memoization stuff

-- | Create a table
tabulate       :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate rng f = listArray rng (map f $ range rng)

-- | Memoize a function using an Array
memo     :: Ix i => (i,i) -> (i -> a) -> (i -> a)
memo rng = (!) . tabulate rng

-- see: for the idea of this memoization
-- https://byorgey.wordpress.com/2023/06/06/dynamic-programming-in-haskell-automatic-memoization/
