--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Set.Util where

import           Data.DynamicOrd
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Internal as Internal


import           Data.Ord (comparing)

data S = S String deriving Show
cmpS :: S -> S -> Ordering
cmpS = comparing (\(S s) -> length s)


-- $setup
-- >>> import Data.Ord(comparing)
-- >>> data S = S String deriving Show
-- >>> cmpS = comparing (\(S s) -> length s)
--

-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
--
-- running time: \(O(\log n)\)
splitOn     :: Ord b => (a -> b) -> b -> Set a -> (Set a, Set a, Set a)
splitOn f b = splitBy (\y -> f y `compare` b)

-- | Given a monotonic function f that orders @a@, split the sequence @s@
-- into three parts. I.e. the result (lt,eq,gt) is such that
-- * all (\x -> f x == LT) . fmap f $ lt
-- * all (\x -> f x == EQ) . fmap f $ eq
-- * all (\x -> f x == GT) . fmap f $ gt
--
-- running time: \(O(\log n)\)
splitBy       :: (a -> Ordering) -> Set a -> (Set a, Set a, Set a)
splitBy f s = genericSplitBy Set.spanAntitone f s

  -- let (l,s') = Set.spanAntitone ((==) LT . f) s
  --                 (m,r)  = Set.spanAntitone ((==) EQ . f) s'
  --             in (l,m,r)

-- | Generic implementation of a ordered splitting function on
-- sequences.  The first argument is a 'spanAntitone' function that
-- splits the sequence based on a monotonic predicate that flips from
-- True to false only once.
--
-- Given a monotonic function f that orders @a@, split the sequence @s@
-- into three parts. I.e. the result (lt,eq,gt) is such that
-- * all (\x -> f x == LT) . fmap f $ lt
-- * all (\x -> f x == EQ) . fmap f $ eq
-- * all (\x -> f x == GT) . fmap f $ gt
--
-- running time: \(O(1)\)*cost of the antitone function.
genericSplitBy :: ( (a -> Bool) -> seq a -> (seq a, seq a) )
               -- ^ the spanAntitone function to use
               -> (a -> Ordering)
               -- ^ a monotonic ordering function f
               -> seq a -> (seq a, seq a, seq a)
genericSplitBy spanAntitone f s = let (l,s') = spanAntitone ((==) LT . f) s
                                      (m,r)  = spanAntitone ((==) EQ . f) s'
                                  in (l,m,r)

-- | Constructs a Set using the given Order.
--
-- Note that this is dangerous as the resulting set may not abide the
-- ordering expected of such sets.
--
-- running time: \(O(n\log n)\)
fromListBy        :: (a -> a -> Ordering) -> [a] -> Set a
fromListBy cmp xs = withOrd cmp (extractOrd1 . Set.fromList . map O $ xs)

-- | Given two sets l and r, such that all elements of l occur before
-- r, join the two sets into a combined set.
--
-- running time: \(O(\log n)\)
join :: Set a -> Set a -> Set a
join = Internal.merge


-- | Inserts an element into the set, assuming that the set is ordered
-- by the given order.
--
-- >>> insertBy cmpS (S "ccc") $ fromListBy cmpS [S "a" , S "bb" , S "dddd"]
-- fromList [S "a",S "bb",S "ccc",S "dddd"]
--
-- When trying to insert an element that equals an element already in
-- the set (according to the given comparator), this function replaces
-- the old element by the new one:
--
-- >>> insertBy cmpS (S "cc") $ fromListBy cmpS [S "a" , S "bb" , S "dddd"]
-- fromList [S "a",S "cc",S "dddd"]
--
-- running time: \(O(\log n)\)
insertBy         :: (a -> a -> Ordering) -> a -> Set a -> Set a
insertBy cmp x s = withOrd cmp $ liftOrd1 (Set.insert $ O x) s


-- | Deletes an element from the set, assuming the set is ordered by
-- the given ordering.
--
-- >>> deleteAllBy cmpS (S "bb") $ fromListBy cmpS [S "a" , S "bb" , S "dddd"]
-- fromList [S "a",S "dddd"]
-- >>> deleteAllBy cmpS (S "bb") $ fromListBy cmpS [S "a" , S "bb" , S "cc", S "dd", S "ee", S "ff", S "dddd"]
-- fromList [S "a",S "dddd"]
--
-- running time: \(O(\log n)\)
deleteAllBy         :: (a -> a -> Ordering) -> a -> Set a -> Set a
deleteAllBy cmp x s = withOrd cmp $ liftOrd1 (Set.delete $ O x) s

-- | Run a query, eg. lookupGE, on the set with the given ordering.
--
-- Note: The 'Algorithms.BinarySearch.binarySearchIn' function may be
-- a useful alternative to 'queryBy'
--
-- >>> queryBy cmpS Set.lookupGE (S "22") $ fromListBy cmpS [S "a" , S "bbb" , S "ddddddd"]
-- Just (S "bbb")
-- >>> queryBy cmpS Set.lookupLE (S "22") $ fromListBy cmpS [S "a" , S "bbb" , S "ddddddd"]
-- Just (S "a")
-- >>> queryBy cmpS Set.lookupGE (S "333") $ fromListBy cmpS [S "a" , S "bbb" , S "ddddddd"]
-- Just (S "bbb")
queryBy           :: (a -> a -> Ordering)
                  -> (forall b. Ord b => b -> Set b -> t b)
                  -> a -> Set a -> t a
queryBy cmp fs q s = withOrd cmp $ liftOrd1 (fs $ O q) s




-- queryBy'           :: Ord r
--                    => (a -> r)
--                    -> r
--                   -> (forall b. Ord b => b -> Set b -> t b)
--                   -> a -> Set a -> t a
-- queryBy' g fs q s = queryBy
--   where



--   withOrd cmp $ liftOrd1 (fs $ O q) s



  -- withOrd cmp $ liftOrd1 (Set.lookupGE $ O q) s




-- test = queryBy cmpS Set.lookupGE (S "22") $ fromListBy cmpS [S "a" , S "bbb" , S "ddddddd"]
-- test = succBy cmpS (S "22") $ fromListBy cmpS [S "a" , S "bbb" , S "ddddddd"]
