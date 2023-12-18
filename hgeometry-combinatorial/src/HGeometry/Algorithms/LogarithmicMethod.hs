{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Algorithms.LogarithmicMethod
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Algorithms.LogarithmicMethod
  ( InsertionOnly(..)
  , empty
  , LogarithmicMethodDS(..)
  , insert
  , queryWith
  )
  where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Foldable1

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

instance LogarithmicMethodDS static a => Semigroup (InsertionOnly static a) where
  (InsertionOnly ds1) <> (InsertionOnly ds2) = InsertionOnly $ runMergeWith Nothing ds1 0 ds2 0

instance LogarithmicMethodDS static a => Monoid (InsertionOnly static a) where
  mempty = empty


-- | Class representing data structures that can be constructed using
-- the Logarithmic method.
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
                        | otherwise -> Just ds1 : dss -- cons -- I don't think insert can ever
                                                              -- trigger this scenario.

-- | merges two structures (potentially with a carry)
--
-- invariant: size carry == size ds1 <= size ds2
runMergeWith                ::  LogarithmicMethodDS static a
                            => Maybe (static a) -- ^ carry, if it exists
                            -> [Maybe (static a)] -> Power
                            -- ^ size of the first ds
                            -> [Maybe (static a)] -> Power
                            -- ^ size of the second ds
                            -> [Maybe (static a)]
runMergeWith mc ds1 i ds2 j = case (ds1,ds2) of
  ([],_)            -> case mc of
                         Nothing  -> ds2
                         Just c   -> runMerge c i j ds2
  (_,[])            -> case mc of
                         Nothing  -> ds1
                         Just c   -> runMerge c i i ds1
  (m1:ds1',m2:ds2') -> case (m1,m2) of
    (Nothing,Nothing) -> mc : runMergeWith Nothing ds1' (i+1) ds2' (j+1)
    (Nothing,Just d2) -> case mc of
         Nothing | i == j    -> m2 : runMergeWith Nothing ds1' (i+1) ds2' (j+1)
                 | otherwise -> m1 : runMergeWith Nothing ds1' (i+1) ds2 j
         Just c  | i == j    -> Nothing : runMergeWith (Just $ c `merge` d2) ds1' (i+1) ds2' (j+1)
                 | otherwise -> mc : runMergeWith Nothing ds1' (i+1) ds2 j
                   -- i < j, so invariant (i+1) <= j again holds holds

    (Just d1,Nothing) -> case mc of
                           Nothing -> m1 : runMergeWith Nothing ds1' (i+1) ds2' (j+1)
                           Just c  -> Nothing : runMergeWith (Just $ c `merge` d1) ds1' (i+1) ds2' (j+1)

    (Just d1,Just d2) -> case mc of
         Nothing | i == j    -> Nothing : runMergeWith (Just $ d1 `merge` d2) ds1' (i+1) ds2' (j+1)
                 | otherwise -> m1      : runMergeWith Nothing ds1' (i+1) ds2 j

         Just c  | i == j    -> mc : runMergeWith (Just $ d1 `merge` d2) ds1' (i+1) ds2' (j+1)
                 | otherwise -> Nothing : runMergeWith (Just $ c `merge` d1) ds1' (i+1) ds2 j
                       -- i < j, so invariant holds

-- | Given a decomposable query algorithm for the static structure,
-- lift it to a query algorithm on the insertion only structure.
--
-- pre: (As indicated by the Monoid constraint), the query answer
-- should be decomposable. I.e. we should be able to anser the query
-- on a set \(A \cup B\) by answering the query on \(A\) and \(B\)
-- separately, and combining their results.
--
-- running time: \(O(Q(n)\log n)\), where \(Q(n)\) is the query time
-- on the static structure.
queryWith                           :: Monoid m => (static a -> m) -> InsertionOnly static a -> m
queryWith query (InsertionOnly dss) = foldMap (foldMap query) dss
