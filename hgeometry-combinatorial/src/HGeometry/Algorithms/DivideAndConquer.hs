--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Algorithms.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Algorithms.DivideAndConquer(
    divideAndConquer
  , divideAndConquer1
  , divideAndConquer1With

  , chunksOf, chunksOf'

  , mergeSorted, mergeSortedLists
  , mergeSortedBy
  , mergeSortedListsBy
  ) where

import           Data.Bifunctor
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

-- | Divide and conquer strategy
--
-- the running time satifies T(n) = 2T(n/2) + M(n),
--
-- where M(n) is the time corresponding to the semigroup operation of s on n elements.
--
--
divideAndConquer1 :: (Foldable1 f, Semigroup s) => (a -> s) -> f a -> s
divideAndConquer1 = divideAndConquer1With (<>)

-- | Divide and conquer strategy. See 'divideAndConquer1'.
divideAndConquer   :: (Foldable f, Monoid s) => (a -> s) -> f a -> s
divideAndConquer g = maybe mempty (divideAndConquer1 g) . NonEmpty.nonEmpty . F.toList

{- HLINT ignore divideAndConquer1With -}
-- | Divide and conquer strategy
--
-- the running time satifies T(n) = 2T(n/2) + M(n),
--
-- where M(n) is the time corresponding to the semigroup operation of s on n elements.
--
divideAndConquer1With         :: Foldable1 f => (s -> s -> s) -> (a -> s) -> f a -> s
divideAndConquer1With (<.>) g = repeatedly merge . fmap g . toNonEmpty
  where
    repeatedly _ (t :| []) = t
    repeatedly f ts        = repeatedly f $ f ts

    merge ts@(_ :| [])  = ts
    merge (l :| r : []) = l <.> r :| []
    merge (l :| r : ts) = l <.> r <| merge (NonEmpty.fromList ts)


--------------------------------------------------------------------------------

-- | Creates chunks of the given size. The last one may be shorter
--
-- pre: n > 0
--
-- >>> chunksOf 3 (0 :| [1..10])
-- (0 :| [1,2]) :| [3 :| [4,5],6 :| [7,8],9 :| [10]]
chunksOf   :: Foldable1 f => Int -> f a -> NonEmpty.NonEmpty (NonEmpty.NonEmpty a)
chunksOf n = NonEmpty.fromList . chunksOf' n
-- note: the fromList is safe, since there must be at least one chunk

-- | Creates chunks of the given size. The last one may be shorter
--
-- pre: n > 0
--
-- >>> chunksOf' 3 [1..10]
--[1 :| [2,3],4 :| [5,6],7 :| [8,9],10 :| []]
chunksOf'   :: Foldable f => Int -> f a -> [NonEmpty.NonEmpty a]
chunksOf' n = go . F.toList
  where
    go xs = case first NonEmpty.nonEmpty (List.splitAt n xs) of
              (Nothing, _)       -> []
              (Just chunk, rest) -> chunk : go rest

--------------------------------------------------------------------------------
-- * Merging NonEmpties/Sorted lists

-- | Merges two sorted non-Empty lists in linear time.
mergeSorted :: Ord a => NonEmpty a -> NonEmpty a -> NonEmpty a
mergeSorted = mergeSortedBy compare

-- | Merges two sorted lists in linear time.
mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists = mergeSortedListsBy compare

-- | Given an ordering and two nonempty sequences ordered according to that
-- ordering, merge them.
--
-- running time: \(O(n*T)\), where \(n\) is the length of the list,
-- and \(T\) the time required to compare two elements.
mergeSortedBy           :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a -> NonEmpty a
mergeSortedBy cmp ls rs = NonEmpty.fromList
                        $ mergeSortedListsBy cmp (NonEmpty.toList ls) (NonEmpty.toList rs)

-- | Given an ordering and two nonempty sequences ordered according to that
-- ordering, merge them
--
-- running time: \(O(n*T)\), where \(n\) is the length of the list,
-- and \(T\) the time required to compare two elements.
mergeSortedListsBy     :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeSortedListsBy cmp = go
  where
    go []         ys     = ys
    go xs         []     = xs
    go xs@(x:xs') ys@(y:ys') = case x `cmp` y of
                                 LT -> x : go xs' ys
                                 EQ -> x : go xs' ys
                                 GT -> y : go xs  ys'
