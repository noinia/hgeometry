module Data.Set.Util where

import           Data.DynamicOrd
import           Data.Reflection
import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Data.Set.Internal as Internal


-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
splitOn       :: Ord b => (a -> b) -> b -> Set a -> (Set a, Set a, Set a)
splitOn f x s = let (l,s') = Set.spanAntitone (g LT . f) s
                    (m,r)  = Set.spanAntitone (g EQ . f) s'
                    g c y  = y `compare` x == c
                in (l,m,r)

-- | Constructs a Set using the given Order.
--
-- Note that this is dangerous as the resulting set may not abide the
-- ordering expected of such sets.
fromListBy        :: (a -> a -> Ordering) -> [a] -> Set a
fromListBy cmp xs = withOrd cmp (liftOrd1 . Set.fromList . map O $ xs)

-- | Given two sets l and r, such that all elements of l occur before
-- r, join the two sets into a combined set.
join :: Set a -> Set a -> Set a
join = Internal.merge
