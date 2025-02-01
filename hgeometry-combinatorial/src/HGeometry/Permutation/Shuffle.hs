--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Permutation.Shuffle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implements Fishyer-Yates shuffle.
--
--------------------------------------------------------------------------------
module HGeometry.Permutation.Shuffle
  ( shuffle
  , shuffleSeq
  ) where

import           Control.Lens (singular,ix,(&),(%%~),bimap)
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import           System.Random
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.MVector as Builder

import           Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import           HGeometry.Sequence.NonEmpty (ViewR1(..))
--------------------------------------------------------------------------------

-- | Fisher–Yates shuffle, which shuffles a list/foldable uniformly at random.
--
-- running time: \(O(n)\).
shuffle      :: forall vector gen a f.
                (V.Vector vector a, Foldable f, RandomGen gen) => gen -> f a -> vector a
shuffle gen0 = construct . Builder.foldable
  where
    construct b = V.create $ do
                    v <- Builder.build b
                    for_ swaps $ \(i,j) ->
                      MV.swap v i j
                    pure v
      where
        swaps = List.unfoldr f (pred $ Builder.size b, gen0)
        f (i,gen)
          | i < 1     = Nothing
          | otherwise = Just . bimap (i,) (pred i,) $ uniformR (0,i) gen

-- | "Inside-out" version of Fissher-Yates shuffle that returns a Seq.  see
-- https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_%22inside-out%22_algorithm
-- for details.
--
-- O(n\log n)
shuffleSeq      :: (RandomGen gen, Foldable f) => gen -> f a -> Seq.Seq a
shuffleSeq gen0 = (\(Acc _ _ s) -> s) . foldl' step (Acc 0 gen0 mempty)
  where
    setAndRetrieve i x s = s&singular (ix i) %%~ (,x)
    -- sets the value at position i to x, and retrieves its current value.

    step (Acc i gen s) x = let (j,gen')     = uniformR (0,i) gen
                               (y,s' :>> _) = setAndRetrieve j x (s :>> x)
                           in Acc (succ i) gen' (s' |> y)
    -- main idea: for every next element x at position i, we generate a random index j <=
    -- i and place x at position j, and store the element y that was at position j at the
    -- new position i

data Acc gen s = Acc {-#UNPACK#-}!Int gen s
