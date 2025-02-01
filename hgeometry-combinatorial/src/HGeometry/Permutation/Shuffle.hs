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
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import           System.Random
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.MVector as Builder

import           Data.Sequence ((|>),(<|),Seq(..))
import qualified Data.Sequence as Seq
import           HGeometry.Sequence.NonEmpty (ViewR1(..))

import qualified Data.IntMap as IntMap

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
    setAndRetrieve i x s = s&singular (ix i) %%~ (,x)
    build s gen = \case
      Empty             -> s
      (remaining :|> x) -> let i              = length remaining
                               (j,gen')       = uniformR (0,i) gen
                               (y,remaining')
                                 | i /= j     = setAndRetrieve j x remaining
                                 | otherwise  = (x,remaining)
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
      -- s&singular (ix i) %%~ (,x)
    step (Acc gen s) x = let (j,gen') = uniformR (0,length s) gen
                             (y,s')   = setAndRetrieve j x s
                         in Acc gen' (s' |> y)
    -- main idea: for every next element x at position i, we generate a random index j <=
    -- i and place x at position j, and store the element y that was at position j at the
    -- new position i

data Acc gen s = Acc !gen !s


-- setAndRetrieve                       :: Int -> a -> Seq.Seq a -> (a, Seq.Seq a)
-- setAndRetrieve i0 x (Internal.Seq t) = Internal.Seq <$> go i0 t
--   where
--     go i = \case
--       Internal.EmptyT   -> error "empty"
--       Internal.Single y -> (y, Internal.Single x) -- i better be zero here ...
--       Internal.Deep n pr m sf
--         | i < spr   -> (\pr' -> Internal.Deep n pr' m sf) <$> goDigit i pr
--         | i < spm   -> (\m'  -> Internal.Deep n pr m' sf) <$> goDeep m
--         | otherwise -> (\sf' -> Internal.Deep n pr m sf') <$> goDigit (i - spm) sf

--     goDigit i = \case
--       Internal.One   a       -> (a, Internal.One x)
--       Internal.Two   a b     -> case i of
--                          0 -> (a, Internal.Two x b)
--                          _ -> (b, Internal.Two a x)
--       Internal.Three a b c   -> case i of
--                          0 -> (a, Internal.Three x b c)
--                          1 -> (b, Internal.Three a x c)
--                          _ -> (c, Internal.Three a b x)
--       Internal.Four  a b c d -> case i of
--                          0 -> (a, Internal.Four x b c d)
--                          1 -> (b, Internal.Four a x c d)
--                          2 -> (c, Internal.Four a c x d)
--                          _ -> (d, Internal.Four a b c x)

--     goDeep i =



--------------------------------------------------------------------------------



-- | "Inside-out" version of Fissher-Yates shuffle that returns a Seq.  see
-- https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_%22inside-out%22_algorithm
-- for details.
--
-- O(n\log n)
shuffleSeqInOutOrig      :: (RandomGen gen, Foldable f) => gen -> f a -> Seq.Seq a
shuffleSeqInOutOrig gen0 = (\(AccOrig _ _ s) -> s) . foldl' step (AccOrig 0 gen0 mempty)
  where
    -- | sets the value at position i to x, and retrieves its current value.
    setAndRetrieve i x s = s&singular (ix i) %%~ (,x)
    step (AccOrig i gen s) x = let (j,gen')     = uniformR (0,i) gen
                                   (y,s' :>> _) = setAndRetrieve j x (s :>> x)
                               in AccOrig (succ i) gen' (s' |> y)
    -- main idea: for every next element x at position i, we generate a random index j <=
    -- i and place x at position j, and store the element y that was at position j at the
    -- new position i

data AccOrig gen s = AccOrig {-#UNPACK#-}!Int gen s


--------------------------------------------------------------------------------
