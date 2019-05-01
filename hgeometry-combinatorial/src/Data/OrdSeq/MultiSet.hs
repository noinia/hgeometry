--------------------------------------------------------------------------------
-- |
-- Module      :  Data.OrdSeq.MultiSet
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of an Ordered Sequence using a MultiSet
--
--------------------------------------------------------------------------------
module Data.OrdSeq.MultiSet where

import           Control.Lens (bimap)
import qualified Data.Foldable as F
import           Data.Maybe
import qualified Data.MultiSet as MultiSet
import           Data.MultiSet (MultiSet)

--------------------------------------------------------------------------------

newtype Elem s a = Elem { getElem :: a } deriving (Traversable,Foldable,Functor)

instance Show a => Show (Elem a) where
  show (Elem x) = show x

newtype OrdSeq a = OrdSeq (MultiSet (Elem () a))
                 deriving (Show,Eq)

asMultiSet (OrdSeq s) = s

instance Semigroup (OrdSeq a) where
  (OrdSeq s) <> (OrdSeq t) = OrdSeq $ s `mappend` t

instance Monoid (OrdSeq a) where
  mempty = OrdSeq mempty
  mappend = (<>)

instance Foldable OrdSeq where
  foldMap f = foldMap (foldMap f) . asMultiSet
  null      = null . asMultiSet
  length    = length . _asFingerTree
  minimum   = fromJust . lookupMin
  maximum   = fromJust . lookupMax

type Compare a = a -> a -> Ordering

-- | Insert into a monotone OrdSeq.
--
-- pre: the comparator maintains monotonicity
--
-- \(O(\log n)\)
insertBy                  :: Compare a -> a -> OrdSeq a -> OrdSeq a
insertBy cmp x (OrdSeq s) = OrdSeq $ l `mappend` (Elem x <| r)
  where
    (l,r) = split (\v -> liftCmp cmp v (Key x) `elem` [EQ, GT]) s

-- | Insert into a sorted OrdSeq
--
-- \(O(\log n)\)
insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert = insertBy compare

deleteAllBy         :: Compare a -> a -> OrdSeq a -> OrdSeq a
deleteAllBy cmp x s = l <> r
  where
    (l,_,r) = splitBy cmp x s

    -- (l,m) = split (\v -> liftCmp cmp v (Key x) `elem` [EQ,GT]) s
    -- (_,r) = split (\v -> liftCmp cmp v (Key x) == GT) m


-- | \(O(\log n)\)
splitBy                  :: Compare a -> a -> OrdSeq a -> (OrdSeq a, OrdSeq a, OrdSeq a)
splitBy cmp x (OrdSeq s) = (OrdSeq l, OrdSeq m', OrdSeq r)
  where
    (l, m) = split (\v -> liftCmp cmp v (Key x) `elem` [EQ,GT]) s
    (m',r) = split (\v -> liftCmp cmp v (Key x) == GT) m


-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
--
-- >>> splitOn id 3 $ fromAscList' [1..5]
-- (OrdSeq {_asFingerTree = fromList [Elem 1,Elem 2]},OrdSeq {_asFingerTree = fromList [Elem 3]},OrdSeq {_asFingerTree = fromList [Elem 4,Elem 5]})
-- >>> splitOn fst 2 $ fromAscList' [(0,"-"),(1,"A"),(2,"B"),(2,"C"),(3,"D"),(4,"E")]
-- (OrdSeq {_asFingerTree = fromList [Elem (0,"-"),Elem (1,"A")]},OrdSeq {_asFingerTree = fromList [Elem (2,"B"),Elem (2,"C")]},OrdSeq {_asFingerTree = fromList [Elem (3,"D"),Elem (4,"E")]})
--
-- \(O(\log n)\)
splitOn :: Ord b => (a -> b) -> b -> OrdSeq a -> (OrdSeq a, OrdSeq a, OrdSeq a)
splitOn f x (OrdSeq s) = (OrdSeq l, OrdSeq m', OrdSeq r)
  where
    (l, m) = split (\(Key v) -> compare (f v) x `elem` [EQ,GT]) s
    (m',r) = split (\(Key v) -> compare (f v) x ==     GT)      m

-- | Given a monotonic predicate p, splits the sequence s into two sequences
--  (as,bs) such that all (not p) as and all p bs
--
-- \(O(\log n)\)
splitMonotonic  :: (a -> Bool) -> OrdSeq a -> (OrdSeq a, OrdSeq a)
splitMonotonic p = bimap OrdSeq OrdSeq . split (p . getKey) . _asFingerTree


-- Deletes all elements from the OrdDeq
--
-- \(O(n\log n)\)
deleteAll :: Ord a => a -> OrdSeq a -> OrdSeq a
deleteAll = deleteAllBy compare


-- | inserts all eleements in order
-- \(O(n\log n)\)
fromListBy     :: Compare a -> [a] -> OrdSeq a
fromListBy cmp = foldr (insertBy cmp) mempty

-- | inserts all eleements in order
-- \(O(n\log n)\)
fromListByOrd :: Ord a => [a] -> OrdSeq a
fromListByOrd = fromListBy compare

-- | O(n)
fromAscList' :: [a] -> OrdSeq a
fromAscList' = OrdSeq . fromList . fmap Elem


-- | \(O(\log n)\)
lookupBy         :: Compare a -> a -> OrdSeq a -> Maybe a
lookupBy cmp x s = let (_,m,_) = splitBy cmp x s in listToMaybe . F.toList $ m

memberBy        :: Compare a -> a -> OrdSeq a -> Bool
memberBy cmp x = isJust . lookupBy cmp x


-- | Fmap, assumes the order does not change
-- O(n)
mapMonotonic   :: (a -> b) -> OrdSeq a -> OrdSeq b
mapMonotonic f = fromAscList' . map f . F.toList


-- | Gets the first element from the sequence
-- \(O(1)\)
viewl :: OrdSeq a -> ViewL OrdSeq a
viewl = f . FT.viewl . _asFingerTree
  where
    f EmptyL         = EmptyL
    f (Elem x :< s)  = x :< OrdSeq s

-- Last element
-- \(O(1)\)
viewr :: OrdSeq a -> ViewR OrdSeq a
viewr = f . FT.viewr . _asFingerTree
  where
    f EmptyR         = EmptyR
    f (s :> Elem x)  = OrdSeq s :> x


-- \(O(1)\)
minView   :: OrdSeq a -> Maybe (a, OrdSeq a)
minView s = case viewl s of
              EmptyL   -> Nothing
              (x :< t) -> Just (x,t)

-- \(O(1)\)
lookupMin :: OrdSeq a -> Maybe a
lookupMin = fmap fst . minView

-- \(O(1)\)
maxView   :: OrdSeq a -> Maybe (a, OrdSeq a)
maxView s = case viewr s of
              EmptyR   -> Nothing
              (t :> x) -> Just (x,t)

-- \(O(1)\)
lookupMax :: OrdSeq a -> Maybe a
lookupMax = fmap fst . maxView
