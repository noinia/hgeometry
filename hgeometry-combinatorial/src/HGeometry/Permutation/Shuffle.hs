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
  , shuffleIntMap
  , shuffleSeq
  , shuffleSeqInOut
  , shuffleSeqInOutOrig
  ) where

import           Control.Lens (singular,ix,(&),(%%~),bimap)
import           Data.Foldable
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import           System.Random
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.MVector as Builder

import           Data.Sequence ((|>),(<|),Seq(..))
import qualified Data.Sequence as Seq
-- import           HGeometry.Sequence.NonEmpty (ViewR1(..))

import qualified Data.IntMap.Strict as IntMap

-- import qualified Data.Sequence.Internal as Internal

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

-- | Returns a strict IntMap
shuffleIntMap      :: (RandomGen gen, Foldable f) => gen -> f a -> IntMap.IntMap a
shuffleIntMap gen0 = build . IntMap.fromList . zip [0..] . toList
  where
    build m = foldl' swap m $ List.unfoldr f (pred $ IntMap.size m, gen0)
      where
        f (i,gen)
          | i < 1     = Nothing
          | otherwise = Just . bimap (i,) (pred i,) $ uniformR (0,i) gen

    swap m (i,j) = IntMap.insert i (m IntMap.! j) . IntMap.insert j (m IntMap.! i) $ m


-- | Version of Fissher-Yates shuffle that returns a Seq.
--
-- O(n\log n)
shuffleSeq      :: (RandomGen gen, Foldable f) => gen -> f a -> Seq.Seq a
shuffleSeq gen0 = build mempty gen0 . foldMap Seq.singleton
  where
    setAndRetrieve i x s = s&singular (ix i) %%~ \y -> SP y x
    build s gen = \case
      Empty             -> s
      (remaining :|> x) -> let i              = length remaining
                               (j,gen')       = uniformR (0,i) gen
                               (SP y remaining')
                                 | i /= j     = setAndRetrieve j x remaining
                                 | otherwise  = SP x remaining
                           in build (y <| s) gen' remaining'

--------------------------------------------------------------------------------


-- | "Inside-out" version of Fissher-Yates shuffle that returns a Seq.  see
-- https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_%22inside-out%22_algorithm
-- for details.
--
-- O(n\log n)
shuffleSeqInOut      :: (RandomGen gen, Foldable f) => gen -> f a -> Seq.Seq a
shuffleSeqInOut gen0 = (\(Acc _ s) -> s) . foldl' step (Acc gen0 mempty)
  where
    -- | sets the value at position i to x, and retrieves its current value.
    setAndRetrieve i x s = case s Seq.!? i of
      Nothing -> (x,s)
      Just y  -> (y,Seq.update i x s)
    step (Acc gen s) x = let (j,gen') = uniformR (0,length s) gen
                             (y,s')   = setAndRetrieve j x s
                         in Acc gen' (s' |> y)
    -- main idea: for every next element x at position i, we generate a random index j <=
    -- i and place x at position j, and store the element y that was at position j at the
    -- new position i

data Acc gen s = Acc !gen !s



--------------------------------------------------------------------------------

-- | "Inside-out" version of Fissher-Yates shuffle that returns a Seq.  see
-- https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_%22inside-out%22_algorithm
-- for details.
--
-- O(n\log n)
shuffleSeqInOutOrig      :: (RandomGen gen, Foldable f) => gen -> f a -> Seq.Seq a
shuffleSeqInOutOrig gen0 = (\(SP _ s) -> s) . foldl' step (SP gen0 mempty)
  where
    -- | sets the value at position i to x, and retrieves its current value.
    setAndRetrieve i x s = s&ix i %%~ \y -> SP (First $ Just y) x
      -- the SP here is very important; if we use a lazy pair this about 4x lower
      -- same for the ! on y below here.

    step (SP gen s) x = let (j,gen') = uniformR (0,length s) gen
                            SP my s' = setAndRetrieve j x s
                            !y       = fromMaybe x $ getFirst my
                        in SP gen' (s' |> y)
    -- main idea: for every next element x at position i, we generate a random index j <=
    -- i and place x at position j, and store the element y that was at position j at the
    -- new position i

--------------------------------------------------------------------------------

data SP a b = SP !a !b
  deriving (Functor)

instance Monoid a => Applicative (SP a) where
  pure x             = SP mempty x
  SP a f <*> SP a' x = SP (a <> a') (f x)
