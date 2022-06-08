{-# LANGUAGE  ScopedTypeVariables  #-}
module Algorithms.Geometry.SPM.PSQueueUtil where

import Prelude hiding (dropWhile)
import Data.Coerce
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.PSQueue.Internal
import Data.Set.Util (genericSplitBy)

import Test.Hspec

--------------------------------------------------------------------------------



-- | Given a monotonic function f that maps a to b, split the sequence s
-- depending on the b values. I.e. the result (l,m,r) is such that
-- * all (< x) . fmap f $ l
-- * all (== x) . fmap f $ m
-- * all (> x) . fmap f $ r
--
-- running time: \(O(\log n)\)
splitBy   :: forall k p. (Ord k, Ord p)
          => (k -> Ordering)
          -> PSQ k p
          -> (PSQ k p, PSQ k p, PSQ k p)
splitBy f = coerce . genericSplitBy spanAntitone' f . Flip2
  where
    -- spanAntitone' :: (k -> Bool) -> (Flip2 PSQ p) k -> ((Flip2 PSQ p) k, (Flip2 PSQ p) k)
    spanAntitone' f' (Flip2 q) = coerce $ spanAntitone f' q

newtype Flip2 f b a = Flip2 (f a b)



--------------------------------------------------------------------------------
-- * The rest could go into the PSQueue package itself

-- | Splits the PSQ into two PSQ's based on a predicate that is
-- antitone, monotonic, i.e. has a single place where it flips from
-- True to False.
--
-- \(O(\log n)\)
spanAntitone   :: (Ord k, Ord p) => (k -> Bool) -> PSQ k p -> (PSQ k p, PSQ k p)
spanAntitone f = go
  where
    go q = case tourView q of
             Null                         -> (Void,Void)
             Single k _ | f k             -> (q,Void)
                        | otherwise       -> (Void,q)
             tl `Play` tr | f (maxKey tl) -> let (tlr,trr) = go tr
                                             in (tl `join` tlr,trr)
                          | otherwise     -> let (tll,trl) = go tl
                                             in (tll, trl `join` tr)

-- | Drops elements from the left
--
dropWhile            :: (Ord k, Ord p) => (k -> p -> Bool) -> PSQ k p -> PSQ k p
dropWhile shouldDrop = fromMaybe Void . go
  where
    go q = case tourView q of
             Null                        -> Just q
             Single k p | shouldDrop k p -> Nothing
                        | otherwise      -> Just q
             tl `Play` tr                -> case go tl of
               Nothing  -> go tr
               Just tl' -> Just $ tl' `join` tr


-- | Drops elements from the right
--
dropWhileR            :: (Ord k, Ord p) => (k -> p -> Bool) -> PSQ k p -> PSQ k p
dropWhileR shouldDrop = fromMaybe Void . go
  where
    go q = case tourView q of
             Null                        -> Just q
             Single k p | shouldDrop k p -> Nothing
                        | otherwise      -> Just q
             tl `Play` tr                -> case go tr of
               Nothing  -> go tl
               Just tr' -> Just $ tl `join` tr'

-- | Get the leftmost binding, i.e. the one with minimum k value.
--
-- \(O(\log n)\)
viewLeftMost   :: (Ord k, Ord p) => PSQ k p -> Maybe (Binding k p, PSQ k p)
viewLeftMost q = case tourView q of
  Null         -> Nothing
  Single k p   -> Just (k :-> p, Void)
  tl `Play` tr -> viewLeftMost tl <&> \(m,tl') -> (m,tl' `play` tr)


-- | Splits the Queue at the given key
--
-- \(O(\log n)\)
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


--------------------------------------------------------------------------------

-- |  test if the first tree is too heavy compared to the second one.
isTooHeavy       :: LTree k p -> LTree k p -> Bool
isTooHeavy t1 t2 = size' t1 > omega * size' t2


-- | decide if the two trees are sufficiently balanced
isBalanced     :: LTree k p -> LTree k p -> Bool
isBalanced t1 t2 = size' t1 <= omega * size' t2
                && size' t2 <= omega * size' t1
  -- TODO: verify


----------------------------------------

-- | Joins two arbitrary PSQ's. This is essentially a more powerful
-- version of 'play' that is applicable even if the two input PSQ's
-- have widely different sizes.
--
-- pre: all kyes in the first tree are smaller than the keys in the
-- second tree.
join                         :: (Ord k, Ord p)
                             => PSQ k p -> PSQ k p -> PSQ k p
join = play


testQ, testQ2 :: PSQ Char Int
testQ = fromList [ 'a' :-> 1
                 , 'b' :-> 2
                 , 'c' :-> 0
                 , 'd' :-> 5
                 , 'e' :-> 100
                 ]

testQ2 = fromList [ 'g' :-> 101
                  , 'h' :-> -1
                  , 'i' :-> 20
                  ]



spec = describe "PSQUtil tests" $ do
  it "spanAntitone" $
    let (fs,ts) = spanAntitone (< 'c') testQ
    in (toList fs, toList ts) `shouldBe`
       (['a' :-> 1,'b' :-> 2],['c' :-> 0,'d' :-> 5,'e' :-> 100])

  it "splitBy" $
    let (l,m,r) = splitBy (\x -> compare x 'c') testQ
    in (toList l, toList m, toList r) `shouldBe`
       (['a' :-> 1,'b' :-> 2],['c' :-> 0],['d' :-> 5,'e' :-> 100])

  it "join" $ do
    toList (testQ `join` testQ2) `shouldBe`
      ['a' :-> 1,'b' :-> 2,'c' :-> 0,'d' :-> 5,'e' :-> 100,'g' :-> 101,'h' :-> -1,'i' :-> 20]
    let Just (a,b) = minView $ testQ `join` testQ2
    (a, toList b) `shouldBe`
      ('h' :-> -1
      ,['a' :-> 1,'b' :-> 2,'c' :-> 0,'d' :-> 5,'e' :-> 100,'g' :-> 101,'i' :-> 20]
      )

  it "dropwhile" $
    toList (dropWhile (\k _ -> k <= 'c') testQ)
    `shouldBe`
    ['d' :-> 5,'e' :-> 100]
  it "dropwhileR" $
    toList (dropWhileR (\k _ -> k >= 'c') testQ)
    `shouldBe`
    ['a' :-> 1,'b' :-> 2]


-- test = splitBy (compare 'c') testQ
test = spanAntitone (< 'c') testQ

{-








join Void              t'    = t'
join t                 Void  = t
join (Winner k p t m) (Winner k' p' t' m')
  | p <= p'   = Winner k  p  (rbalanceJoin k' p' t m t') m'
  | otherwise = Winner k' p' (lbalanceJoin k  p  t m t') m'

-- | Balance after joining two tree, remember the root came from the
-- right subtree.
--
-- construct a balanced loser tree representing the above tree.
rbalanceJoin       :: (Ord k, Ord p)
                   => k     -- ^ the key k of the best loser from the right
                   -> p     -- ^ the priority p of the best loser from the right
                   -> LTree k p   -- ^ the left subtree l of losers
                   -> k          -- ^ the maximum key m from the left subtree l
                   -> LTree k p  -- ^ the right subtree. The binding (k,p)
                                 -- is the best loser from this right subtree.
                   -> LTree k p
rbalanceJoin k p l m r
  | isTooHeavy l r = rBalanceJoinRight k p l m r
                     -- left is much heavier than right
                     -- , so attach (k,p) and r to the right spine of tl
  | isTooHeavy r l = rBalanceJoinLeft  k p l m r -- symmetric to the above.
  | otherwise      = rloser            k p l m r
                     -- both are sufficiently balanced, so directly create the node

-- | Attaches the root+right subtree onto the right spine of the left
-- subtree.
--
-- pre: the left subtree is much larger than the right subtree
rBalanceJoinRight :: (Ord k, Ord p) => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
rBalanceJoinRight k p tl m tr
  | isBalanced tl tr = rloser k p tl m tr
  | otherwise        = let l  = left tl
                           c  = right tl
                           t' = rBalanceJoinRight k p c m tr
                       in go l t'
  where
    go l t' | like (size' l) (size' t') = rloser k' p' l' t' -- wy is this an rloser ?
            | like (size' l) (size' l1)
              &&
              like (size' l + size' l1) r1 = undefined -- rrotateLeft l' k' p' t' ?
            | otherwise = undefined
    -- ah, supposedly this is a single double-rotation. so maybe we can use
    -- rdoubleLeft

    -- I we may have to swap some values here as well


like a b = a < omega * b


  -- = case tl of



  -- | size' l + size' r < 2     = rloser        k p l m r
  -- -- | size' r > omega * size' l = rbalanceLeft  k p l m r -- TODO continue insert left
  -- -- | size' l > omega * size' r = rbalanceRight k p l m r -- TODO continue insert right
  -- | otherwise                 =


rBalanceJoinLeft = undefined





-- | given a key, priority, and two trees + key, create a balanced
-- tree whose root is a LeftNode
lbalanceJoin :: (Ord k, Ord p) => k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lbalanceJoin = undefined


--------------------------------------------------------------------------------

-}
