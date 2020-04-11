module Algorithms.BinarySearch where

import Data.Sequence(Seq, ViewL(..),ViewR(..))
import qualified Data.Sequence as S
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
binarySearchUntil       :: (Fractional r, Ord r, Show r)
                        => r
                        -> (r -> Bool) -> r -> r -> r
binarySearchUntil eps p = go
  where
    go l u | u - l < eps = u
           | otherwise   = let m = (l + u) / 2
                           in if p m then go l m else go m u


--------------------------------------------------------------------------------

-- | Given a monotonic predicate, Get the index h such that everything strictly
-- smaller than h has: p i = False, and all i >= h, we have p h = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(\log^2 n + T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchSeq     :: (a -> Bool) -> Seq a -> Maybe Int
binarySearchSeq p s = case S.viewr s of
                       EmptyR                 -> Nothing
                       (_ :> x)   | p x       -> Just $ case S.viewl s of
                         (y :< _) | p y          -> 0
                         _                       -> binarySearch p' 0 u
                                  | otherwise -> Nothing
  where
    p' = p . S.index s
    u  = S.length s - 1

-- | Given a monotonic predicate, get the index h such that everything strictly
-- smaller than h has: p i = False, and all i >= h, we have p h = True
--
-- returns Nothing if no element satisfies p
--
-- running time: \(O(T*\log n)\), where \(T\) is the time to execute the
-- predicate.
binarySearchVec                             :: V.Vector v a
                                            => (a -> Bool) -> v a -> Maybe Int
binarySearchVec p' v | V.null v   = Nothing
                     | not $ p n' = Nothing
                     | otherwise  = Just $ if p 0 then 0
                                                  else binarySearch p 0 n'
  where
    n' = V.length v - 1
    p = p' . (v V.!)
