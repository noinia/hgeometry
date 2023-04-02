{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Algorithms.BinarySearch
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--------------------------------------------------------------------------------
module HGeometry.Algorithms.BinarySearch where

import           Control.Applicative ((<|>))
import           Data.Kind
import           Data.Sequence (Seq, ViewL(..),ViewR(..))
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
-- Get the index h such that everything strictly smaller than h has: p i =
-- False, and all i >= h, we have p h = True
--
-- running time: \(O(\log(u - l))\)
{-# SPECIALIZE binarySearch :: (Int -> Bool) -> Int -> Int -> Int #-}
{-# SPECIALIZE binarySearch :: (Word -> Bool) -> Word -> Word -> Word #-}
binarySearch   :: Integral a => (a -> Bool) -> a -> a -> a
binarySearch p = go
  where
    go l u = let d = u - l
                 m = l + (d `div` 2)
             in if d == 1 then u else if p m then go l m
                                             else go m u

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

-- | Containers storing elements on which we can binary search.
class BinarySearch v where
  -- | The type of the elements of the container
  type Elem  v :: Type
  -- | The type of indices used in the container.
  type Index v :: Type

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
  binarySearchIn     :: (Elem v -> Bool) -> v -> Maybe (Elem v)

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
  binarySearchIdxIn :: (Elem v -> Bool) -> v -> Maybe (Index v)

--------------------------------------------------------------------------------
-- * Searching on a Sequence

instance BinarySearch (Seq a) where
  type Index (Seq a) = Int
  type Elem  (Seq a) = a

  -- ^ runs in \(O(T*\log^2 n)\) time.
  binarySearchIn p s = Seq.index s <$>  binarySearchIdxIn p s
  {-# INLINABLE binarySearchIn #-}

  -- ^ runs in \(O(T*\log^2 n)\) time.
  binarySearchIdxIn p s = case Seq.viewr s of
                            EmptyR                 -> Nothing
                            (_ :> x)   | p x       -> Just $ case Seq.viewl s of
                              (y :< _) | p y          -> 0
                              _                       -> binarySearch p' 0 u
                                       | otherwise -> Nothing
    where
      p' = p . Seq.index s
      u  = Seq.length s - 1
  {-# INLINABLE binarySearchIdxIn #-}


instance {-# OVERLAPPABLE #-} V.Vector v a => BinarySearch (v a) where
  type Index (v a) = Int
  type Elem  (v a) = a

  binarySearchIdxIn p' v | V.null v   = Nothing
                         | not $ p n' = Nothing
                         | otherwise  = Just $ if p 0 then 0 else binarySearch p 0 n'
    where
      n' = V.length v - 1
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
        Set.Tip                     -> Nothing
        Set.Bin _ k l r | p k       -> go l <|> Just k
                        | otherwise -> go r
  {-# INLINABLE binarySearchIn #-}

  binarySearchIdxIn p = go
    where
      go = \case
        Set.Tip                     -> Nothing
        Set.Bin _ k l r | p k       -> go l <|> Just (Set.size l)
                        | otherwise -> (+ (1 + Set.size l)) <$> go r
  {-# INLINABLE binarySearchIdxIn #-}
