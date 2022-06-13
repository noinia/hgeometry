module Algorithms.Graph.ShortestPath
  ( shortestPaths
  , WithPath(..)
  ) where

import           Control.Lens
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.PSQueue (Binding(..))
import qualified Data.PSQueue as PSQueue
import           Data.Semigroup
import           Data.UnBounded

--------------------------------------------------------------------------------

-- | Dijkstra's algorithm for computing the shortest paths from the
-- given source node to all other nodes in the unweighted graph
shortestPaths           :: forall f g v w. (Foldable f, Foldable g
                                           , Ord v
                                           , Ord w, Monoid w)
                        => (v -> v -> Top w) -- ^ weight function
                        -> v -- ^ source
                        -> f (v, g v)    -- ^ adjacency graph
                        -> [(v, Top w)]
shortestPaths weight s graph = go . initializeQueue $ graph
  where
    initializeQueue :: f (v, g v) -> Queue g v w
    initializeQueue = PSQueue.adjust (fmap (const mempty)) s
                    . PSQueue.fromList
                    . map (\(u,ns) -> u :-> WithNeighbours Top ns)
                    . F.toList

    go       :: Queue g v w -> [(v,Top w)]
    go queue = case PSQueue.minView queue of
      Nothing                                      -> []
      Just (u :-> WithNeighbours du neighs,queue') -> (u, du) : go (relaxAll u du queue' neighs)

    relaxAll      :: v -> Top w -> Queue g v w -> g v -> Queue g v w
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

shortestPaths'         :: (Ord v, Monoid r, Ord r) => v -> WeightedGraph v r -> [(v, Top r)]
shortestPaths' s graph =  shortestPaths (weightF graph) s (dropWeights graph)

type WeightedGraph v r = [(v,[(v,r)])]


weightF           :: Eq v => WeightedGraph v r -> v -> v -> Top r
weightF graph u v = review _TopMaybe $ lookup u graph >>= lookup v

dropWeights :: WeightedGraph v r -> [(v,[v])]
dropWeights = map (\(v,vs) -> (v,map fst vs))

testGraph :: WeightedGraph Char (Sum Int)
testGraph = coerce $
            [ ('s', [('a',5 :: Int),('b',10)])
            , ('a', [('s',5),('b',1),('c',6)])
            , ('b', [('s',10),('a',1),('c',4)])
            , ('c', [('a',6),('b',4)])
            ]

test1 = shortestPaths' 's' testGraph

test2 = shortestPaths' 's' testGraph
