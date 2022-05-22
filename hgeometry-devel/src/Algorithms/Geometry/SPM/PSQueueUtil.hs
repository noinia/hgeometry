module Algorithms.Geometry.SPM.PSQueueUtil where

import Data.PSQueue.Internal


-- | Joins two PSQ's
--
-- pre: all kyes in the first tree are smaller than the keys in the second tree.
join                            :: PSQ k p -> PSQ k p -> PSQ k p
join tl tr | size tl <= size tr = joinL tl tr
           | otherwise          = joinR tl tr

-- | Join's tl onto tr by "inserting" tl along the left spine of tr
joinL tl tr = case tourView tl of
                Null       -> tr
                Single k p -> insert k p tl
                _          -> go
  -- where
  --   go = case tourView tr of
  --          Null -> tl
  --          Single


joinR tl tr = undefined


-- | Splits the Queue at the given key
--
-- O(log n)
split   :: Ord k => k -> PSQ k p -> (PSQ k p, Maybe (Binding k p), PSQ k p)
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
