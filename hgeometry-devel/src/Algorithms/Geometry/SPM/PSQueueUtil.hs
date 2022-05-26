{-# LANGUAGE  ScopedTypeVariables  #-}
module Algorithms.Geometry.SPM.PSQueueUtil where

import Data.Coerce
import Data.PSQueue.Internal
import Data.Set.Util (genericSplitBy)


--------------------------------------------------------------------------------



-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
--
-- running time: \(O(\log n)\)
splitBy   :: forall k p. (k -> Ordering) -> PSQ k p
          -> (PSQ k p, PSQ k p, PSQ k p)
splitBy f = coerce . genericSplitBy spanAntitone' f . Flip2
  where
    spanAntitone' :: (k -> Bool) -> (Flip2 PSQ p) k -> ((Flip2 PSQ p) k, (Flip2 PSQ p) k)
    spanAntitone' f' (Flip2 q) = coerce $ spanAntitone (\k _ -> f' k) q

newtype Flip2 f b a = Flip2 (f a b)



--------------------------------------------------------------------------------
-- * The rest could go into the PSQueue package itself




-- | Splits the PSQ into two PSQ's based on a predicate that is
-- antitone, monotonic, i.e. has a single place where it flips from
-- True to False.
spanAntitone   :: (k -> p -> Bool) -> PSQ k p -> (PSQ k p, PSQ k p)
spanAntitone f = undefined

-- |
dropWhile :: (k -> p -> Bool) -> PSQ k p -> PSQ k p
dropWhile f = undefined


-- | Joins two PSQ's
--
-- pre: all kyes in the first tree are smaller than the keys in the second tree.
join                         :: (Ord k, Ord p)
                             => PSQ k p -> PSQ k p -> PSQ k p
-- join = play

join Void              t'    = t'
join t                 Void  = t
join (Winner k p t m) (Winner k' p' t' m')
  | p <= p'   = Winner k  p  (rbalanceJoin k' p' t m t') m'
  | otherwise = Winner k' p' (lbalanceJoin k  p  t m t') m'

-- | given a key, priority, and two trees + key, create a balanced
-- tree whose root is a RightNode
rbalanceJoin :: (Ord k, Ord p) => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rbalanceJoin k p l m r
  | size' l + size' r < 2     = rloser        k p l m r
  -- | size' r > omega * size' l = rbalanceLeft  k p l m r -- continue insert left
  -- | size' l > omega * size' r = rbalanceRight k p l m r -- continue insert right
  | otherwise                 = rloser        k p l m r




-- | given a key, priority, and two trees + key, create a balanced
-- tree whose root is a LeftNode
lbalanceJoin :: (Ord k, Ord p) => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lbalanceJoin = undefined




-- | Splits the Queue at the given key
--
-- O(log n)
split   :: (Ord k, Ord p) => k -> PSQ k p -> (PSQ k p, Maybe (Binding k p), PSQ k p)
split k = go
  where
    go q = case tourView q of
             Null         -> (Void, Nothing, Void)
             Single k' p  -> case k `compare` k' of
                                 LT -> (Void, Nothing, q)
                                 EQ -> (Void, Just (k :-> p), Void)
                                 GT -> (q, Nothing, Void)
             tl `Play` tr
               | k <= maxKey tl -> let (tl',x,tr') = go tl
                                   in (tl', x, tr' `join` tr)
               | otherwise      -> let (tl',x, tr') = go tr
                                   in (tl `join` tl', x, tr')
