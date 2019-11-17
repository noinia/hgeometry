module Algorithms.DivideAndConquer( divideAndConquer
                                  , divideAndConquer1
                                  , divideAndConquer1With

                                  , mergeSorted, mergeSortedLists
                                  , mergeSortedBy
                                  , mergeSortedListsBy
                                  ) where

import           Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NonEmpty


-- | Divide and conquer strategy
--
-- the running time is: O(n*L) + T(n) = 2T(n/2) + M(n)
--
-- where M(n) is the time corresponding to the semigroup operation of s
--
divideAndConquer1 :: Semigroup s => (a -> s) -> NonEmpty a -> s
divideAndConquer1 = divideAndConquer1With (<>)


-- | Divide and conquer for
divideAndConquer   :: Monoid s => (a -> s) -> [a] -> s
divideAndConquer g = maybe mempty (divideAndConquer1 g) . NonEmpty.nonEmpty

-- | Divide and conquer strategy
--
-- the running time is: O(n*L) + T(n) = 2T(n/2) + M(n)
--
-- where M(n) is the time corresponding to the merge operation s
--
divideAndConquer1With         :: (s -> s -> s) -> (a -> s) -> NonEmpty a -> s
divideAndConquer1With (<.>) g = repeatedly merge . fmap g
  where
    repeatedly _ (t :| []) = t
    repeatedly f ts        = repeatedly f $ f ts

    merge ts@(_ :| [])  = ts
    merge (l :| r : []) = l <.> r :| []
    merge (l :| r : ts) = l <.> r <| (merge $ NonEmpty.fromList ts)


--------------------------------------------------------------------------------
-- * Merging NonEmpties/Sorted lists

mergeSorted :: Ord a => NonEmpty a -> NonEmpty a -> NonEmpty a
mergeSorted = mergeSortedBy compare

mergeSortedLists :: Ord a => [a] -> [a] -> [a]
mergeSortedLists = mergeSortedListsBy compare

-- | Given an ordering and two nonempty sequences ordered according to that
-- ordering, merge them
mergeSortedBy           :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a -> NonEmpty a
mergeSortedBy cmp ls rs = NonEmpty.fromList
                        $ mergeSortedListsBy cmp (NonEmpty.toList ls) (NonEmpty.toList rs)


-- | Given an ordering and two nonempty sequences ordered according to that
-- ordering, merge them
mergeSortedListsBy     :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeSortedListsBy cmp = go
  where
    go []         ys     = ys
    go xs         []     = xs
    go xs@(x:xs') ys@(y:ys') = case x `cmp` y of
                                 LT -> x : go xs' ys
                                 EQ -> x : go xs' ys
                                 GT -> y : go xs  ys'
