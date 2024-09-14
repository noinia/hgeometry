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
  ( Separator(..)
  , planarSeparator

  , bff
  , connectedComponents

  , treeEdges
  , graphEdges
  ) where

import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (Down(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Type
import           HGeometry.Plane.LowerEnvelope.Connected.Split
import           HGeometry.Vector

import           Debug.Trace
--------------------------------------------------------------------------------
-- * BFS

-- | Computes a breath first forest
-- (O(n \log n))
bff    :: Ord k => PlaneGraph k v e -> [Tree k]
bff gr = go (Map.keysSet gr)
  where
    go remaining = case Set.minView remaining of
      Nothing              -> []
      Just (v, remaining') -> let (remaining'', t) = bfs gr v remaining'
                              in t : go remaining''

-- | Turn the map into a tree.
toTree   :: Ord k => Map k [k] -> k -> Tree k
toTree m = go
  where
    go s = Node s $ map go (fromMaybe [] $ Map.lookup s m)

-- | BFS from the given starting vertex, and the set of still-to-visit vertices.
-- returns the remaining still-to-visit vertices and the tree.
bfs      :: Ord k => PlaneGraph k v e -> k -> Set k -> (Set k, Tree k)
bfs gr s = fmap (flip toTree s) . bfs' [s] Map.empty
  where
    bfs' lvl m remaining = case foldr visit ([], remaining, m) lvl of
                             (lvl', remaining', m') -> case lvl' of
                               [] -> (remaining',m')
                               _  -> bfs' lvl' m' remaining'

    visit v (lvl,remaining, m) = let chs = filter (flip Set.member remaining) $ neighs v
                                 in ( chs <> lvl
                                    , foldr Set.delete remaining chs
                                    , Map.insert v chs m
                                    )
    neighs v = maybe [] (Map.elems . fst) $ Map.lookup v gr

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

data LevelInfo v = Level { levelIndex  :: {-# UNPACK #-}!Int
                         , levelSize   :: {-# UNPACK #-}!Int
                         -- ^ size of this level
                         , accumSize :: {-# UNPACK #-}!Int
                         -- ^ size of the prefix up to this level
                         , levelVertices :: [v]
                         }
                 deriving (Show,Eq,Foldable)

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

-- planarSeparator    :: PlanarGraph_ planarGraph
--                    => planarGraph
--                    -> ([VertexIx planarGraph], Vector 2 [VertexIx planarGraph])

type Size = Int

-- | Computes the connected components; for each component we report a BFS tree. The trees
-- are reported on decreasing size
connectedComponents :: Ord k => PlaneGraph k v e -> [(Tree k,Size)]
connectedComponents = List.sortOn (Down . snd) . map (\t -> (t, length t)) . bff




-- | Returns a pair (separator, Vector2 verticesSubGraphA verticesSubGraphB)
-- so that
--
-- 1) there are no edges connecting subGraph A and subgraph B,
-- 2) the size of the separator is at most sqrt(n).
-- 3) the vertex sets of A and B have weight at most 2/3 the total weight
planarSeparator    :: ( Ord k
                      , Show k
                      ) => PlaneGraph k v e -> Separator [k]
planarSeparator gr = case trees of
    []                 -> mempty
    ((tr,m):rest)
      | m <= twoThirds -> traceShow (tr,m,n,twoThirds) $ groupComponents
      | otherwise      -> planarSeparator' tr m -- FIXME: we should also add the remaining vertices
  where
    trees = connectedComponents gr
    n     = sum $ map snd trees
    half = n `div` 2
    twoThirds = 2 * (n `div` 3)

    groupComponents = undefined

    planarSeparator' tr _ = case List.break (\lvl -> accumSize lvl < half) lvls of
        (_,    [])          -> Separator [] (F.toList tr) []
                                 -- somehow we have too little weight;
        (pref, (l1 : suff)) -> planarSeparatorTree twoThirds gr tr'
          where
            k      = accumSize l1
            p  lvl = levelSize lvl + 2*(levelIndex l1  - levelIndex lvl)    <= 2 * sqrt' k
            p' lvl = levelSize lvl + 2*(levelIndex lvl - levelIndex l1 - 1) <= 2 * sqrt' (n-k)

            l0 = findR     p  (pref <> [l1])
            l2 = List.find p' suff
            tr' = trim l0 l2 tr


            -- sep       = undefined
            -- verticesA = undefined
            -- verticesB = undefined
      where
            -- compute the levels, their sizes, and the sum of their sizes
        (_, lvls) = List.mapAccumL (\(Vector2 i acc) lvl ->
                                       let m    = length lvl
                                           acc' = acc + m
                                       in ( Vector2 (i+1) acc', Level i m acc' lvl)
                                   ) (Vector2 0 0) $ levels tr


-- | contracts the plane graph so that we get a spanning tree of diameter at most sqrt(n).
contract :: PlaneGraph k v e -> Tree k -> (PlaneGraph k v e, Tree k)
contract = undefined

trim _ _ tr = tr
-- TODO:

--------------------------------------------------------------------------------

-- | Find the last element matching some predicate
findR   :: (a -> Bool) -> [a] -> Maybe a
findR p = List.find p . reverse



-- if the input graph is connected, are subgraph A and SubGraphB then connected?
-- I don't think so; in particular; the "outer layers", so Graph B may be disconnected I guess.


-- data Separators planarGraph =


-- planarSeparators :: PlanarGraph_ planarGraph
--                  => planarGraph -> Tree
