module Algorithms.Geometry.SPM.PSQueueUtil where

import Data.PSQueue.Internal


-- | Joins two PSQ's
--
-- pre: all kyes in the first tree are smaller than the keys in the second tree.
join :: (Ord k, Ord p) => PSQ k p -> PSQ k p -> PSQ k p
join = play
-- FIXME!!! seems that this is essentially just 'play'. However, I
-- don't think play does the right balancing for this type of
-- operation.




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
