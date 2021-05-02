module Algorithms.Graph.BFS
  (

  ) where

import           Control.Monad.ST.Strict
import           Data.Semigroup.Bifoldable
import           Data.Tree
import qualified Data.Vector.NonEmpty as V
import qualified Data.Vector.Unboxed.Mutable as UMV

-- | Runs a BFS from the first vertex in the graph. The graph is given
-- in adjacency list represnetation.
bfs      :: Foldable f => Int -> V.NonEmptyVector (v, f Int) -> Tree v
bfs s gr = do st <- UMV.replicate n False
              go0 st q mempty
  where
    n = V.length gr
    go0 st q mempty = go q mempty
      where
        go u queue = do let (u,ns) = gr V.! u
                        mark u
                        ns' <- filterM unmarked ns
                        Node u <$> case queue <> ns' of
                                     Empty          -> pure []
                                     (v :<| queue') -> go v queue'

        mark i   = UMV.write st i False
        unmarked = UMV.read st
