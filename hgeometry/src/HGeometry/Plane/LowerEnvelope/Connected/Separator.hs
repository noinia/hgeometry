{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator
  (
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Plane.LowerEnvelope.Connected.Graph

--------------------------------------------------------------------------------


data LevelInfo v = Level { levelIndex  :: {-# UNPACK #-}!Int
                         , levelSize   :: {-# UNPACK #-}!Int
                         -- ^ size of this level
                         , acccumSize :: {-# UNPACK #-}!Int
                         -- ^ size of the prefix up to this level
                         , levelVertices :: [v]
                         }
                 deriving (Show,Eq,Foldable)




type PlanarGraph vertex = Map vertex [vertex]


{-
-- | Returns a pair (separator, Vector2 verticesSubGraphA verticesSubGraphB)
-- so that
--
-- 1) there are no edges connecting subGraph A and subgraph B,
-- 2) the size of the separator is at most sqrt(n).
-- 3) the vertex sets of A and B have weight at most 2/3 the total weight
planarSeparator    :: PlanarGraph_ planarGraph
                   => planarGraph
                   -> ([VertexIx planarGraph], Vector 2 [VertexIx planarGraph])
planarSeparator gr = case List.break (\lvl -> accumSize lvl < half) lvls of
    (pref, [])           -> ([], gr^..vertices.asIndex, [])
                            -- somehow we have too little weight;
    (pref, suff@(l1 : _) ->
         let k   = accumSize l1
             ls0 = List.takeWhile (\lvl ->
                                      levelSize lvl + 2*(levelIndex l1 - levelIndex lvl)                                                   <= 2 * sqrt' k) pref
              ls2 = undefined
         in (sep, Vector2 verticesA verticesB)
  where
    sep       = undefined
    verticesA = undefined
    verticesB = undefined

    v0   = gr^.vertices.head1
    tr   = bfs gr v0

    lZero = 0

    -- compute the levels, their sizes, and the sum of their sizes
    (_, lvls) = List.mapAccumL (\(Vector2 i acc) lvl ->
                                   let m    = length lvl
                                       acc' = acc + m
                                   in ( Vector2 (i+1) acc', Level i m acc' lvl)
                               ) (Vector2 0 0) $ levels tr

    half = n `div` 2
    n = numVertices gr

-}




-- if the input graph is connected, are subgraph A and SubGraphB then connected?
-- I don't think so; in particular; the "outer layers", so Graph B may be disconnected I guess.


-- data Separators planarGraph =


-- planarSeparators :: PlanarGraph_ planarGraph
--                  => planarGraph -> Tree
