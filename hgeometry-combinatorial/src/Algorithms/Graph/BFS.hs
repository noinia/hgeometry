{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Graph.BFS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Graph.BFS
  ( bfs
  , bfs'
  ) where

import           Control.Monad.ST.Strict
import qualified Data.Foldable as F
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Tree
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Witherable

--------------------------------------------------------------------------------

-- | Runs a BFS from the first vertex in the graph. The graph is given
-- in adjacency list representation.
--
-- running time: \(O(V + E)\)
bfs      :: Foldable f => Int -> V.Vector (v, f Int) -> Tree v
bfs s gr = fmap (fst . (gr V.!)) . bfs' s . fmap snd $ gr

-- | Runs a BFS from the first vertex in the graph. The graph is given
-- in adjacency list representation.
--
-- running time: \(O(V + E)\)
bfs'      :: Foldable f => Int -> V.Vector (f Int) -> Tree Int
bfs' s gr = extract s $ V.create
         $ do st  <- UMV.replicate n False
              out <- MV.new n
              go0 st out (s :<| mempty)
              pure out
  where
    n = V.length gr
    go0        :: forall s. UMV.MVector s Bool -> MV.MVector s [Int]
               -> Seq Int -> ST s ()
    go0 st out = go
      where
        visit i = do b <- UMV.read st i
                     UMV.write st i True -- mark i as visited
                     pure $ if b then Nothing else Just i

        go :: Seq Int -> ST s ()
        go = \case
          Empty       -> pure ()
          (u:<|queue) -> do ns <- wither visit . F.toList $ gr V.! u
                            MV.write out u ns -- write that u's children are ns
                            go (queue <> Seq.fromList ns)


-- | Give na root index and a vector s.t. v[i] lists the children of
-- node i, builds the acutal tree.
extract     :: Int -> V.Vector [Int] -> Tree Int
extract s v = go s
  where
    go i = Node i (map go $ v V.! i)
