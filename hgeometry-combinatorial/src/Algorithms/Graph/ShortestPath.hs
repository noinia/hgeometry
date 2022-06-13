module Algorithms.Graph.ShortestPath
  ( shortestPaths, shortestPaths'
  , shortestPathsWith
  , mapWeightFunction

  , WithPath(..)
  , withPaths
  ) where

import           Control.Lens
import           Control.Monad (join)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.Map as Map
import           Data.PSQueue (Binding(..))
import qualified Data.PSQueue as PSQueue
import           Data.Semigroup
import           Data.UnBounded

--------------------------------------------------------------------------------

-- | Given a weighted adjacency graph with finite weights, compute the
-- shortest paths from the source.
shortestPaths         :: forall graph adjList v w.
                         ( Foldable graph, Foldable adjList, Functor graph, Functor adjList
                         , Ord v
                         , Ord w, Monoid w
                         )
                      => v -- ^ source
                      -> graph (v, adjList (v, w))    -- ^ weighted adjacency graph
                      -> [(v, Top w)]
shortestPaths s = shortestPaths' s . mapWeightFunction (const ValT)

-- | Given a weighted adjacency graph with possibly infinite weights,
-- compute the shortest paths from the source
shortestPaths'         :: forall graph adjList v w.
                         ( Foldable graph, Foldable adjList
                         , Ord v
                         , Ord w, Monoid w
                         )
                      => v -- ^ source
                      -> graph (v, adjList (v, Top w))    -- ^ weighted adjacency graph
                      -> [(v, Top w)]
shortestPaths' s graph = shortestPathsWith weightF s (Map.toAscList graph')
  where
    graph' = foldMap (\(u,ns) -> Map.singleton u (mkNeighs ns)) $ graph

    weightF     :: v -> v -> Top w
    weightF u v = join . review _TopMaybe $ Map.lookup u graph' >>= lookupN v

-- | finite weights are also possibly infinite weights
mapWeightFunction   :: (Functor graph, Functor adjList)
                     => (v -> w -> w')
                     -> graph (v, adjList (v, w))    -- ^ weighted adjacency graph
                     -> graph (v, adjList (v, w'))
mapWeightFunction f = fmap (\(u,ns) -> (u, fmap (\(v,w) -> (v, f v w)) ns))


withPaths :: (Functor graph, Functor adjList)
          => graph (v, adjList (v, w))    -- ^ weighted adjacency graph
          -> graph (v, adjList (v, WithPath [] v w))
withPaths = mapWeightFunction (\v w -> WithPath [v] w)


--------------------------------------------------------------------------------

-- | Dijkstra's algorithm for computing the shortest paths from the
-- given source node to all other nodes in the unweighted graph
shortestPathsWith                    :: forall graph adjList v w.
                                    ( Foldable graph, Foldable adjList
                                    , Ord v
                                    , Ord w, Monoid w
                                    )
                                 => (v -> v -> Top w) -- ^ weight function
                                 -> v -- ^ source
                                 -> graph (v, adjList v)    -- ^ adjacency graph
                                 -> [(v, Top w)]
shortestPathsWith weight s graph = go . initializeQueue $ graph
  where
    initializeQueue :: graph (v, adjList v) -> Queue adjList v w
    initializeQueue = PSQueue.adjust (fmap (const $ ValT mempty)) s
                    . PSQueue.fromList
                    . map (\(u,ns) -> u :-> WithNeighbours Top ns)
                    . F.toList

    go       :: Queue adjList v w -> [(v,Top w)]
    go queue = case PSQueue.minView queue of
      Nothing                                      -> []
      Just (u :-> WithNeighbours du neighs,queue') -> (u, du) : go (relaxAll u du queue' neighs)

    relaxAll      :: v -> Top w -> Queue adjList v w -> adjList v -> Queue adjList v w
    relaxAll u du = foldr (\v -> decreasePrio v $ du <> weight u v)

--------------------------------------------------------------------------------

type Queue g v r = PSQueue.PSQ v (WithNeighbours (g v) (Top r))

-- | Decrease the key
decreasePrio     :: (Ord k, Ord p, Functor f, Ord (f p)) => k -> p
                 -> PSQueue.PSQ k (f p) -> PSQueue.PSQ k (f p)
decreasePrio k p = PSQueue.adjust (fmap (min p)) k


-- | Attach neighbours to the priorities
data WithNeighbours vs p = WithNeighbours { thePrio  :: !p
                                          , _neighs  :: vs
                                          }
                         deriving (Functor)

instance Eq p => Eq (WithNeighbours vs p) where
  (==) = (==) `on` thePrio
instance Ord p => Ord (WithNeighbours vs p) where
  compare = compare `on` thePrio

--------------------------------------------------------------------------------

data WithPath f v r = WithPath { path  :: f v
                               , dist  :: !r
                               } deriving (Show)

instance Eq r => Eq (WithPath f v r) where
  (==) = (==) `on` dist
instance Ord r => Ord (WithPath f v r) where
  compare = compare `on` dist

instance (Semigroup r, Semigroup (f v)) => Semigroup (WithPath f v r) where
  (WithPath p d) <> (WithPath p' d') = WithPath (p <> p') (d <> d')

instance (Monoid r, Monoid (f v)) => Monoid (WithPath f v r) where
  mempty = WithPath mempty mempty



--------------------------------------------------------------------------------
-- * Helper so that we can run with a weighted graph

newtype Neighs w v = Neighs { unNeighs :: Map.Map v w }

instance Foldable (Neighs w) where
  foldr f z = Map.foldrWithKey (\v _ -> f v) z . unNeighs

lookupN   :: Ord v => v -> Neighs w v -> Maybe w
lookupN k = Map.lookup k . unNeighs

mkNeighs :: (Foldable f, Ord v) => f (v, w) -> Neighs w v
mkNeighs = Neighs . foldMap (uncurry Map.singleton)

--------------------------------------------------------------------------------




type WeightedGraph v r = [(v,[(v,r)])]

testGraph :: WeightedGraph Char (Sum Int)
testGraph = coerce $
            [ ('s', [('a',5 :: Int),('b',10)])
            , ('a', [('s',5),('b',1),('c',6)])
            , ('b', [('s',10),('a',1),('c',4)])
            , ('c', [('a',6),('b',4)])
            ]

test1 = shortestPaths 's' testGraph

test2 = shortestPaths 's' $ withPaths testGraph
