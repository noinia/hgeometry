{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Algorithms.BinarySearch
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--------------------------------------------------------------------------------
module HGeometry.Algorithms.BinarySearch
  ( -- * Generic Binary Search algorithms
    binarySearchFirst
  , binarySearchLast
  , binarySearchUntil

  , BinarySearchResult(..)
  , firstTrue, lastFalse

  ,  BinarySearch(..)
  , binarySearchFirstIn, binarySearchFirstIdxIn
  , binarySearchLastIn, binarySearchLastIdxIn
  ) where

import           Data.Kind
import           Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set.Internal as Set
import qualified Data.Vector.Generic as V
--------------------------------------------------------------------------------

-- | Given a monotonic predicate p, a lower bound l, and an upper bound u, with:
--  p l = False
--  p u = True
--  l < u.
--
-- Get the index h such that:
--
-- - all indices i <  h have p i = False, and
-- - all indices i >= h have p i = True
--
--
-- That is, find the first index h for which the predicate is True.
--
-- running time: \(O(\log(u - l))\)
{-# SPECIALIZE binarySearchFirst :: (Int -> Bool) -> Int -> Int -> Int #-}
{-# SPECIALIZE binarySearchFirst :: (Word -> Bool) -> Word -> Word -> Word #-}
binarySearchFirst   :: Integral a => (a -> Bool) -> a -> a -> a
binarySearchFirst p = go
  where
    go l u = let d = u - l
                 m = l + (d `div` 2)
             in if d == 1 then u else if p m then go l m
                                             else go m u

-- | Given a monotonic predicate p, a lower bound l, and an upper bound u, with:
--  p l = False
--  p u = True
--  l < u.
--
-- Get the index h such that:
--
-- - all indices i <= h have p i = False, and
-- - all indices i >  h have p i = True
--
-- That is, find the last index h for which the predicate is False.
--
-- running time: \(O(\log(u - l))\)
{-# SPECIALIZE binarySearchLast :: (Int -> Bool) -> Int -> Int -> Int #-}
{-# SPECIALIZE binarySearchLast :: (Word -> Bool) -> Word -> Word -> Word #-}
binarySearchLast       :: (Integral a) => (a -> Bool) -> a -> a -> a
binarySearchLast p l u = (binarySearchFirst p l u) - 1

--------------------------------------------------------------------------------

-- | Given a value \(\varepsilon\), a monotone predicate \(p\), and two values \(l\) and
-- \(u\) with:
--
-- - \(p l\) = False
-- - \(p u\) = True
-- - \(l < u\)
--
-- we find a value \(h\) such that:
--
-- - \(p h\) = True
-- - \(p (h - \varepsilon)\) = False
--
-- >>> binarySearchUntil (0.1) (>= 0.5) 0 (1 :: Double)
-- 0.5
-- >>> binarySearchUntil (0.1) (>= 0.51) 0 (1 :: Double)
-- 0.5625
-- >>> binarySearchUntil (0.01) (>= 0.51) 0 (1 :: Double)
-- 0.515625
binarySearchUntil       :: (Fractional r, Ord r)
                        => r
                        -> (r -> Bool) -> r -> r -> r
binarySearchUntil eps p = go
  where
    go l u | u - l < eps = u
           | otherwise   = let m = (l + u) / 2
                           in if p m then go l m else go m u
{-# INLINABLE binarySearchUntil #-}


--------------------------------------------------------------------------------
-- * Binary Searching in some data structure

-- | Data type representing the result of a binary search
data BinarySearchResult a = AllTrue a
                          | FlipsAt a a -- ^ the last false elem and the first true elem
                          | AllFalse (Maybe a) -- ^ A maybe, since the collection may be empty
                          deriving (Show,Eq,Functor,Foldable,Traversable)


-- instance Alternative (BinarySearchResult a) where
--   l@(AllTrue _)   <|> _ = l
--   l@(FlipsAt _ _) <|> _ = l
--   l@(AllFalse m)  <|> r = case r of
--                             AllFalse Nothing -> l
--                             _                -> r

firstTrue :: BinarySearchResult a -> Maybe a
firstTrue = \case
  AllTrue x   -> Just x
  FlipsAt _ x -> Just x
  AllFalse _  -> Nothing

lastFalse  :: BinarySearchResult a -> Maybe a
lastFalse = \case
  AllTrue _    -> Nothing
  FlipsAt x _  -> Just x
  AllFalse mx  -> mx

----------------------------------------

-- | Containers storing elements on which we can binary search.
class BinarySearch v where
  -- | The type of the elements of the container
  type Elem  v :: Type
  -- | The type of indices used in the container.
  type Index v :: Type

  -- | Given a monotonic predicate p and a data structure v, find the pair of
  -- elements (v[h], v[h+1]) such that that
  --
  -- for every index i <= h we have p v[i] = False, and
  -- for every inedx i >  h we have p v[i] = True
  --
  --
  -- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
  -- predicate.
  binarySearchIn     :: (Elem v -> Bool) -> v -> BinarySearchResult (Elem v)

  -- | Given a monotonic predicate p and a data structure v, find the
  -- index h such that that
  --
  -- for every index i <= h we have p v[i] = False, and
  -- for every inedx i >  h we have p v[i] = True
  --
  -- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
  -- predicate.
  binarySearchIdxIn :: (Elem v -> Bool) -> v -> BinarySearchResult (Index v)

----------------------------------------

-- | Given a monotonic predicate p and a data structure v, find the
-- element v[h] such that that
--
-- for every index i <  h we have p v[i] = False, and
-- for every inedx i >= h we have p v[i] = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchFirstIn   :: BinarySearch v => (Elem v -> Bool) -> v -> Maybe (Elem v)
binarySearchFirstIn p = firstTrue . binarySearchIn p
{-# INLINE binarySearchFirstIn #-}

-- | Given a monotonic predicate p and a data structure v, find the
-- index h such that that
--
-- for every index i <  h we have p v[i] = False, and
-- for every inedx i >= h we have p v[i] = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchFirstIdxIn   :: BinarySearch v => (Elem v -> Bool) -> v -> Maybe (Index v)
binarySearchFirstIdxIn p = firstTrue . binarySearchIdxIn p
{-# INLINE binarySearchFirstIdxIn #-}

-- | Given a monotonic predicate p and a data structure v, find the
-- element v[h] such that that
--
-- for every index i <= h we have p v[i] = False, and
-- for every inedx i >  h we have p v[i] = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchLastIn   :: BinarySearch v => (Elem v -> Bool) -> v -> Maybe (Elem v)
binarySearchLastIn p = lastFalse . binarySearchIn p
{-# INLINE binarySearchLastIn #-}

-- | Given a monotonic predicate p and a data structure v, find the
-- index h such that that
--
-- for every index i <= h we have p v[i] = False, and
-- for every inedx i >  h we have p v[i] = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchLastIdxIn   :: BinarySearch v => (Elem v -> Bool) -> v -> Maybe (Index v)
binarySearchLastIdxIn p = lastFalse . binarySearchIdxIn p
{-# INLINE binarySearchLastIdxIn #-}

--------------------------------------------------------------------------------
-- * Searching on a Sequence

instance BinarySearch (Seq a) where
  type Index (Seq a) = Int
  type Elem  (Seq a) = a

  -- ^ runs in \(O(T*\log^2 n)\) time.
  binarySearchIn p s = Seq.index s <$>  binarySearchIdxIn p s
  {-# INLINABLE binarySearchIn #-}

  -- ^ runs in \(O(T*\log^2 n)\) time.
  binarySearchIdxIn p s = case Seq.viewl s of
                            (y :< _) | p y -> binarySearch p' 0 (Seq.length s)
                            _              -> AllFalse Nothing
    where
      p' = p . Seq.index s
  {-# INLINABLE binarySearchIdxIn #-}

-- | Helper to implement binary search on vectors and sequences
binarySearch       :: (Int -> Bool) -> Int -> Int -> BinarySearchResult Int
binarySearch p l u = let h' = binarySearchFirst p l u
                     in if h' == u then AllFalse (Just (u-1))
                                   else FlipsAt (h'-1) h'

instance {-# OVERLAPPABLE #-} V.Vector v a => BinarySearch (v a) where
  type Index (v a) = Int
  type Elem  (v a) = a

  binarySearchIdxIn p' v | V.null v   = AllFalse Nothing
                         | otherwise  = if p 0 then AllTrue 0 else binarySearch p 0 (V.length v)
    where
      p = p' . (v V.!)
  {-# INLINABLE binarySearchIn #-}

  binarySearchIn p v = (v V.!) <$>  binarySearchIdxIn p v
  {-# INLINABLE binarySearchIdxIn #-}

instance BinarySearch (Set a) where
  type Index (Set a) = Int
  type Elem  (Set a) = a

  binarySearchIn p = go
    where
      go = \case
        Set.Tip                     -> AllFalse Nothing
        Set.Bin _ k l r | p k       -> case go l of
                                         AllFalse Nothing  -> AllTrue k
                                         AllFalse (Just x) -> FlipsAt x k
                                         res               -> res
                        | otherwise -> case go r of
                                         AllFalse Nothing -> AllFalse (Just k)
                                         res              -> res
  {-# INLINABLE binarySearchIn #-}

  binarySearchIdxIn p = go
    where
      go = \case
        Set.Tip                     -> AllFalse Nothing
        Set.Bin _ k l r | p k       -> case go l of
                                         AllFalse Nothing  -> AllTrue 0
                                         AllFalse (Just h) -> FlipsAt h (h+1)
                                         res               -> res
                        | otherwise -> let h = 1 + Set.size l
                                       in case go r of
                                            AllFalse Nothing  -> AllFalse $ Just h
                                            res               -> (+h) <$> res
  {-# INLINABLE binarySearchIdxIn #-}



--------------------------------------------------------------------------------
