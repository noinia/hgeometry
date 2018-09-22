module Data.SlowSeq where


import           Control.Lens (bimap)
-- import qualified Data.FingerTree as FT
-- import           Data.FingerTree hiding (null, viewl, viewr)
import           Data.FingerTree(ViewL(..),ViewR(..))
import qualified Data.Foldable as F
import           Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Sequence.Util as SU



--------------------------------------------------------------------------------

data Key a = NoKey | Key { getKey :: a } deriving (Show,Eq,Ord)

instance Semigroup (Key a) where
  k <> NoKey = k
  _ <> k     = k

instance Monoid (Key a) where
  mempty = NoKey
  k `mappend` k' = k <> k'

liftCmp                     :: (a -> a -> Ordering) -> Key a -> Key a -> Ordering
liftCmp _   NoKey   NoKey   = EQ
liftCmp _   NoKey   (Key _) = LT
liftCmp _   (Key _) NoKey   = GT
liftCmp cmp (Key x) (Key y) = x `cmp` y



-- newtype Elem a = Elem { getElem :: a } deriving (Eq,Ord,Traversable,Foldable,Functor)

-- instance Show a => Show (Elem a) where
--   show (Elem x) = "Elem " <> show x


newtype OrdSeq a = OrdSeq { _asSeq :: S.Seq a }
                   deriving (Show,Eq)

instance Semigroup (OrdSeq a) where
  (OrdSeq s) <> (OrdSeq t) = OrdSeq $ s `mappend` t

instance Monoid (OrdSeq a) where
  mempty = OrdSeq mempty
  mappend = (<>)

instance Foldable OrdSeq where
  foldMap f = foldMap f . _asSeq
  null      = null . _asSeq
  length    = length . _asSeq
  minimum   = fromJust . lookupMin
  maximum   = fromJust . lookupMax

-- instance Measured (Key a) (Elem a) where
--   measure (Elem x) = Key x


type Compare a = a -> a -> Ordering

-- | Insert into a monotone OrdSeq.
--
-- pre: the comparator maintains monotonicity
--
-- \(O(\log^2 n)\)
insertBy                  :: Compare a -> a -> OrdSeq a -> OrdSeq a
insertBy cmp x (OrdSeq s) = OrdSeq $ l `mappend` (x S.<| r)
  where
    (l,r) = split (\v -> cmp v x `elem` [EQ, GT]) s






-- | Insert into a sorted OrdSeq
--
-- \(O(\log^2 n)\)
insert :: Ord a => a -> OrdSeq a -> OrdSeq a
insert = insertBy compare

deleteAllBy         :: Compare a -> a -> OrdSeq a -> OrdSeq a
deleteAllBy cmp x s = l <> r
  where
    (l,_,r) = splitBy cmp x s

    -- (l,m) = split (\v -> liftCmp cmp v (Key x) `elem` [EQ,GT]) s
    -- (_,r) = split (\v -> liftCmp cmp v (Key x) == GT) m


-- | \(O(\log^2 n)\)
splitBy                  :: Compare a -> a -> OrdSeq a -> (OrdSeq a, OrdSeq a, OrdSeq a)
splitBy cmp x (OrdSeq s) = (OrdSeq l, OrdSeq m', OrdSeq r)
  where
    (l, m) = split (\v -> cmp v x `elem` [EQ,GT]) s
    (m',r) = split (\v -> cmp v x == GT) m


-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
--
-- >>> splitOn id 3 $ fromAscList' [1..5]
-- (OrdSeq {_asSeq = fromList [Elem 1,Elem 2]},OrdSeq {_asSeq = fromList [Elem 3]},OrdSeq {_asSeq = fromList [Elem 4,Elem 5]})
-- >>> splitOn fst 2 $ fromAscList' [(0,"-"),(1,"A"),(2,"B"),(2,"C"),(3,"D"),(4,"E")]
-- (OrdSeq {_asSeq = fromList [Elem (0,"-"),Elem (1,"A")]},OrdSeq {_asSeq = fromList [Elem (2,"B"),Elem (2,"C")]},OrdSeq {_asSeq = fromList [Elem (3,"D"),Elem (4,"E")]})
--
-- \(O(\log^2 n)\)
splitOn :: Ord b => (a -> b) -> b -> OrdSeq a -> (OrdSeq a, OrdSeq a, OrdSeq a)
splitOn f x (OrdSeq s) = (OrdSeq l, OrdSeq m', OrdSeq r)
  where
    (l, m) = split (\v -> compare (f v) x `elem` [EQ,GT]) s
    (m',r) = split (\v -> compare (f v) x ==     GT)      m

-- | Given a monotonic predicate p, splits the sequence s into two sequences
--  (as,bs) such that all (not p) as and all p bs
--
-- \(O(\log^2 n)\)
splitMonotonic  :: (a -> Bool) -> OrdSeq a -> (OrdSeq a, OrdSeq a)
splitMonotonic p = bimap OrdSeq OrdSeq . split p . _asSeq


-- monotonic split for Sequences
--
-- \(O(\log^2 n)\)
split :: (a -> Bool) -> S.Seq a -> (S.Seq a, S.Seq a)
split = SU.splitMonotone

-- Deletes all elements from the OrdDeq
--
-- \(O(\log^2 n)\)
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
fromAscList' = OrdSeq . S.fromList


-- | \(O(\log^2 n)\)
lookupBy                  :: Compare a -> a -> OrdSeq a -> Maybe a
lookupBy cmp x s = let (_,m,_) = splitBy cmp x s in listToMaybe . F.toList $ m

memberBy        :: Compare a -> a -> OrdSeq a -> Bool
memberBy cmp x = isJust . lookupBy cmp x


-- | Fmap, assumes the order does not change
-- \(O(n)\)
mapMonotonic   :: (a -> b) -> OrdSeq a -> OrdSeq b
mapMonotonic f = fromAscList' . map f . F.toList


-- | Gets the first element from the sequence
-- \(O(1)\)
viewl :: OrdSeq a -> ViewL OrdSeq a
viewl = f . S.viewl . _asSeq
  where
    f S.EmptyL         = EmptyL
    f (x S.:< s)  = x :< OrdSeq s

-- Last element
-- \(O(1)\)
viewr :: OrdSeq a -> ViewR OrdSeq a
viewr = f . S.viewr . _asSeq
  where
    f S.EmptyR    = EmptyR
    f (s S.:> x)  = OrdSeq s :> x


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
