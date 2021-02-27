{-# LANGUAGE DefaultSignatures #-}
module Algorithms.LogarithmicMethod
  ( InsertionOnly(..)
  , empty
  , LogarithmicMethodDS(..)
  , insert
  , queryWith
  )
  where


import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import           Data.Semigroup.Foldable

--------------------------------------------------------------------------------

-- | Represents an insertion-only data structure built from static
-- data structures.
--
-- In particular, we maintain \(O(\log n)\) static data structures of
-- sizes \(2^i\), for \(i \in [0..c\log n]\).
newtype InsertionOnly static a = InsertionOnly [Maybe (static a)]
  deriving (Show,Eq,Ord)

-- | Builds an empty structure
empty :: InsertionOnly static a
empty = InsertionOnly []

instance Functor static => Functor (InsertionOnly static) where
  fmap f (InsertionOnly dss) = InsertionOnly $ map (fmap (fmap f)) dss

instance Traversable static => Traversable (InsertionOnly static) where
  traverse f (InsertionOnly dss) = InsertionOnly <$> traverse (traverse (traverse f)) dss

instance Foldable static => Foldable (InsertionOnly static) where
  foldMap f = queryWith (foldMap f)
  length = sum . mapMaybe (uncurry (<$)) . withSizes

class LogarithmicMethodDS static a where
  {-# MINIMAL build #-}
  -- | Create a new static data structure storing only one value.
  singleton :: a          -> static a
  singleton = build . (:| [])

  -- | Given a NonEmpty list of a's build a static a.
  build     :: NonEmpty a -> static a

  -- | Merges two structurs of the same size. Has a default
  -- implementation via build in case the static structure is Foldable1.
  merge     :: static a   -> static a -> static a
  default merge :: Foldable1 static => static a -> static a -> static a
  merge as bs = build $ toNonEmpty as <> toNonEmpty bs


type Power = Word

-- | 2^h, for whatever value h.
pow2   :: Integral i => Power -> i
pow2 h = 2 ^ h

-- | Annotate the data structures with their sizes
withSizes                     :: Integral i => InsertionOnly static a -> [(i,Maybe (static a))]
withSizes (InsertionOnly dss) = zipWith (\i ds -> (pow2 i,ds))  [0..] dss

-- | Inserts an element into the data structure
--
-- running time: \(O(M(n)\log n / n)\), where \(M(n)\) is the time
-- required to merge two data structures of size \(n\).
insert                       :: LogarithmicMethodDS static a
                             => a -> InsertionOnly static a -> InsertionOnly static a
insert x (InsertionOnly dss) = InsertionOnly $ runMerge (singleton x) 0 0 dss

-- | Runs the merging procedure. If there are two data structures of
-- the same size they are merged.
runMerge         :: LogarithmicMethodDS static a
                 => static a -- ^ ds1
                 -> Power    -- ^ ds1 has size 2^i
                 -> Power    -- ^ the first entry in the next list corresponds to size 2^j
                 -> [Maybe (static a)] -> [Maybe (static a)]
runMerge ds1 i j = \case
  []                                -> [Just ds1]
  dss@(Nothing  : dss') | i == j    -> Just ds1 : dss' -- replace
                        | otherwise -> Just ds1 : dss  -- cons
  dss@(Just ds2 : dss') | i == j    -> Nothing : runMerge (ds1 `merge` ds2) (i+1) (j+1) dss'
                        | otherwise -> Just ds1 : dss -- cons

-- | Given a query algorithm for the static structure, lift it to a
-- query algorithm on the insertion only structure.
--
-- running time: \(O(Q(n)\log n)\), where \(Q(n)\) is the query time
-- on the static structure.
queryWith                           :: Monoid m => (static a -> m) -> InsertionOnly static a -> m
queryWith query (InsertionOnly dss) = foldMap (foldMap query) dss

--------------------------------------------------------------------------------
-- * Example

newtype DummySucc a = Dummy (NonEmpty a)
  deriving (Show,Eq,Functor,Foldable,Foldable1,Traversable)

instance Ord a => LogarithmicMethodDS DummySucc a where
  build = Dummy . NonEmpty.sort

successor'              :: Ord a => a -> DummySucc a -> Option (Min a)
successor' q (Dummy xs) = case NonEmpty.dropWhile (< q) xs of
                            []    -> Option Nothing
                            (s:_) -> Option (Just (Min s))





successor   :: Ord a => a -> InsertionOnly DummySucc a -> Maybe a
successor q = fmap getMin . getOption . queryWith (successor' q)



fromList :: Ord a => [a] -> InsertionOnly DummySucc a
fromList = foldr insert empty

test = fromList [1,2,3,40,10,4]
