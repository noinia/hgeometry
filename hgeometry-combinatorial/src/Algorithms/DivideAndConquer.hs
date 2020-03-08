module Algorithms.DivideAndConquer( divideAndConquer
                                  , divideAndConquer1
                                  , divideAndConquer1With

                                  , mergeSorted, mergeSortedLists
                                  , mergeSortedBy
                                  , mergeSortedListsBy
                                  ) where

import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable

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
    merge (l :| r : ts) = l <.> r <| (merge $ NonEmpty.fromList ts)


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
