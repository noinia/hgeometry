--------------------------------------------------------------------------------
-- |
-- Module      :  Data.OrdSeq
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.OrdSeq
  ( OrdSeq
  , Compare
  , insertBy
  , insert
  , splitBy
  , splitOn
  , splitMonotonic
  , deleteAll
  , deleteAllBy
  , fromListBy
  , fromListByOrd
  , fromAscList
  , lookupBy
  , memberBy
  , mapMonotonic
  , viewl
  , viewr
  , minView
  , lookupMin
  , maxView
  , lookupMax
  ) where


import           Control.Lens    (bimap)
import           Data.FingerTree hiding (null, viewl, viewr)
import qualified Data.FingerTree as FT
import qualified Data.Foldable   as F
import           Data.Maybe      (fromJust, isJust, listToMaybe)
import           Test.QuickCheck (Arbitrary (arbitrary))

--------------------------------------------------------------------------------

data Key a = NoKey | Key { getKey :: !a } deriving (Show,Eq,Ord)

instance Semigroup (Key a) where
  k <> NoKey = k
  _ <> k     = k

instance Monoid (Key a) where
  mempty = NoKey

liftCmp                     :: (a -> a -> Ordering) -> Key a -> Key a -> Ordering
liftCmp _   NoKey   NoKey   = EQ
liftCmp _   NoKey   (Key _) = LT
liftCmp _   (Key _) NoKey   = GT
liftCmp cmp (Key x) (Key y) = x `cmp` y



newtype Elem a = Elem a deriving (Eq,Ord,Traversable,Foldable,Functor)

instance Show a => Show (Elem a) where
  show (Elem x) = "Elem " <> show x

-- | Sequence of ordered elements.
newtype OrdSeq a = OrdSeq { _asFingerTree :: FingerTree (Key a) (Elem a) }
                   deriving (Eq,Semigroup,Monoid)

instance Show a => Show (OrdSeq a) where
  show s = "fromAscList " ++ show (F.toList s)

instance Foldable OrdSeq where
  foldMap f = foldMap (foldMap f) . _asFingerTree
  null      = null . _asFingerTree
  length    = length . _asFingerTree
  minimum   = fromJust . lookupMin
  maximum   = fromJust . lookupMax

instance (Arbitrary a, Ord a) => Arbitrary (OrdSeq a) where
  arbitrary = fromListByOrd <$> arbitrary

instance Measured (Key a) (Elem a) where
  measure (Elem x) = Key x

-- | Signature for functions that give the ordering of two values.
type Compare a = a -> a -> Ordering

-- | Insert into a monotone OrdSeq.
--
-- pre: the comparator maintains monotonicity
--
-- \(O(\log n)\)
insertBy                  :: Compare a -> a -> OrdSeq a -> OrdSeq a
insertBy cmp x (OrdSeq s) = OrdSeq $ l <> (Elem x <| r)
  where
    (l,r) = split (\v -> liftCmp cmp v (Key x) `elem` [EQ, GT]) s

-- | Insert into a sorted OrdSeq
--
-- \(O(\log n)\)
insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert = insertBy compare

-- | \( O(\log n) \). Delete all elements that compare as equal to @x@.
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


{- HLINT ignore splitOn -}
-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
--
-- * @all (< x) . fmap f $ l@
-- * @all (== x) . fmap f $ m@
-- * @all (> x) . fmap f $ r@
--
-- >>> splitOn id 3 $ fromAscList [1..5]
-- (fromAscList [1,2],fromAscList [3],fromAscList [4,5])
-- >>> splitOn fst 2 $ fromAscList [(0,"-"),(1,"A"),(2,"B"),(2,"C"),(3,"D"),(4,"E")]
-- (fromAscList [(0,"-"),(1,"A")],fromAscList [(2,"B"),(2,"C")],fromAscList [(3,"D"),(4,"E")])
splitOn :: Ord b => (a -> b) -> b -> OrdSeq a -> (OrdSeq a, OrdSeq a, OrdSeq a)
splitOn f x (OrdSeq s) = (OrdSeq l, OrdSeq m', OrdSeq r)
  where
    (l, m) = split (\v -> compare (f' v) x `elem` [EQ,GT]) s
    (m',r) = split (\v -> compare (f' v) x ==     GT)      m

    f' = \case
      NoKey -> error "Data.OrdSeq.splitOn :: trying to match on a NoKey"
      Key v -> f v


-- | Given a monotonic predicate p, splits the sequence s into two sequences
--  (as,bs) such that all (not p) as and all p bs
--
-- \(O(\log n)\)
splitMonotonic  :: (a -> Bool) -> OrdSeq a -> (OrdSeq a, OrdSeq a)
splitMonotonic p = bimap OrdSeq OrdSeq . split (p . getKey) . _asFingerTree


-- | Deletes all elements from the OrdDeq
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

-- | \( O(n) \)
fromAscList :: [a] -> OrdSeq a
fromAscList = OrdSeq . fromList . fmap Elem


-- | \(O(\log n)\)
lookupBy         :: Compare a -> a -> OrdSeq a -> Maybe a
lookupBy cmp x s = let (_,m,_) = splitBy cmp x s in listToMaybe . F.toList $ m

-- | \(O(\log n)\). Queries for the existance of any elements that compare as equal to @x@.
memberBy        :: Compare a -> a -> OrdSeq a -> Bool
memberBy cmp x = isJust . lookupBy cmp x


-- | \( O(n) \) Fmap, assumes the order does not change
mapMonotonic   :: (a -> b) -> OrdSeq a -> OrdSeq b
mapMonotonic f = fromAscList . map f . F.toList


-- | \(O(1)\) Gets the first element from the sequence
viewl :: OrdSeq a -> ViewL OrdSeq a
viewl = f . FT.viewl . _asFingerTree
  where
    f EmptyL        = EmptyL
    f (Elem x :< s) = x :< OrdSeq s

-- | \(O(1)\) Gets the last element from the sequence
viewr :: OrdSeq a -> ViewR OrdSeq a
viewr = f . FT.viewr . _asFingerTree
  where
    f EmptyR        = EmptyR
    f (s :> Elem x) = OrdSeq s :> x


-- | \(O(1)\)
minView   :: OrdSeq a -> Maybe (a, OrdSeq a)
minView s = case viewl s of
              EmptyL   -> Nothing
              (x :< t) -> Just (x,t)

-- | \(O(1)\)
lookupMin :: OrdSeq a -> Maybe a
lookupMin = fmap fst . minView

-- | \(O(1)\)
maxView   :: OrdSeq a -> Maybe (a, OrdSeq a)
maxView s = case viewr s of
              EmptyR   -> Nothing
              (t :> x) -> Just (x,t)

-- | \(O(1)\)
lookupMax :: OrdSeq a -> Maybe a
lookupMax = fmap fst . maxView
