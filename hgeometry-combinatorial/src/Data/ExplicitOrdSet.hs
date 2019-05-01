module Data.ExplicitOrdSet( ExpSet
                          , compareBy
                          , empty
                          , emptyOrdSet

                          , insert
                          , insertAll
                          , delete
                          , split
                          , splitMonotone
                          , viewL
                          , viewR

                          , minimum
                          , maximum
                          ) where

import           Prelude hiding (filter,foldl,foldr,null,map,minimum,maximum)

import qualified Data.Foldable as F
import           Data.Maybe(listToMaybe)
import           Data.Monoid



-- | Set representation with explicit compare function.  to update the
-- comparison of a expset s use s { compareBy = myNewComparisonFunction }.
-- Use this at your own risk, i.e. use it only if the order of the elements is
-- the same for the old and the new comparison function.
data ExpSet a = ExpSet { compareBy :: Cmp a
                       , tree      :: Tree a
                       }

instance F.Foldable ExpSet where
  foldMap f (ExpSet _ t) = F.foldMap f t


withTree                  :: (Cmp a -> Tree a -> Tree a) -> ExpSet a -> ExpSet a
withTree f (ExpSet cmp t) = ExpSet cmp $ f cmp t


type Cmp a = a -> a -> Ordering

data Tree a = Bin {-# UNPACK #-} !Size !(Tree a) !a !(Tree a)
            | Tip
              deriving (Show,Eq)

type Size = Int

instance F.Foldable Tree where
  foldMap _ Tip           = mempty
  foldMap f (Bin _ l x r) = F.foldMap f l <> f x <> F.foldMap f r

--------------------------------------------------------------------------------

-- | Given a comparison function, construct a new set
empty :: Cmp a -> ExpSet a
empty = flip ExpSet Tip

-- | Construct a new set using the standard compare as comparison function.
emptyOrdSet :: Ord a => ExpSet a
emptyOrdSet = empty compare

-- | Insert element
insert   :: a -> ExpSet a -> ExpSet a
insert x = withTree (flip insertT x)

insertAll      :: [a] -> ExpSet a -> ExpSet a
insertAll xs t = F.foldr insert t xs


-- | Delete an element from the set
delete :: a -> ExpSet a -> ExpSet a
delete x = withTree (flip deleteT x)

-- | Split the Set in (elements < x, elements == x, elements > x)
split                  :: a -> ExpSet a -> (ExpSet a, Maybe a, ExpSet a)
split x (ExpSet cmp t) = let (l,m,r) = splitT cmp x t in (ExpSet cmp l, m, ExpSet cmp r)

splitMonotone :: (a -> Bool) -> ExpSet a -> (ExpSet a, ExpSet a)
splitMonotone p (ExpSet cmp t) = let (l,r) = splitMonotoneT cmp p t
                                 in (ExpSet cmp l, ExpSet cmp r)


-- | Elements in left to right oder
viewL :: ExpSet a -> [a]
viewL = F.toList

-- | Elements in right to left order
viewR :: ExpSet a -> [a]
viewR = reverseFoldMap (\x -> [x]) . tree

-- | O(log n)
minimum :: ExpSet a -> Maybe a
minimum = listToMaybe . viewL

-- | O(log n)
maximum :: ExpSet a -> Maybe a
maximum = listToMaybe . viewR

--------------------------------------------------------------------------------

insertT                       :: Cmp a -> a -> Tree a -> Tree a
insertT _   x Tip             = bin Tip x Tip
insertT cmp x t@(Bin _ l y r) = case x `cmp` y of
  LT -> balanceL (insertT cmp x l) y r
  EQ -> t
  GT -> balanceR l y (insertT cmp x r)


join3 :: Cmp a -> Tree a -> a -> Tree a -> Tree a
join3 cmp l                  x Tip = insertT cmp x l
join3 cmp Tip                x r   = insertT cmp x r
join3 cmp l@(Bin _ ll lx lr) x r@(Bin _ rl rx rr)
  | balL && balR = bin l x r
  | balL         = balanceL ll lx (join3 cmp lr x r)
  | otherwise    = balanceR (join3 cmp l x rl) rx rr
  where
    balL = isBalanced l r
    balR = isBalanced r l


-- | The middle parameter is a Just if we removed the x from the tree
--  and Nothing otherwise

splitT                     :: Cmp a -> a -> Tree a -> (Tree a, Maybe a, Tree a)
splitT _   _ Tip           = (Tip,Nothing,Tip)
splitT cmp x (Bin _ l y r) = case x `cmp` y of
  LT -> let (ll,ms,lr) = splitT cmp x l in (ll, ms, join3 cmp lr y r)
  EQ -> (l, Just x, r)
  GT -> let (rl,ms,rr) = splitT cmp x r in (join3 cmp l y rl, ms, rr)

-- | Split based on a monotone predicate p. Returns a pair of trees (l,r) s.t.
-- for all nodes v in l, p v == False, and for all v in r, p v == True.
splitMonotoneT                     :: Cmp a
                                   -> (a -> Bool)
                                   -> Tree a -> (Tree a, Tree a)
splitMonotoneT _   _ Tip    = (Tip,Tip)
splitMonotoneT cmp p (Bin _ l y r)
  | p y       = let (ll,lr) = splitMonotoneT cmp p l in (ll, join3 cmp lr y r)
  | otherwise = let (rl,rr) = splitMonotoneT cmp p r in (join3 cmp l y rl, rr)


extractMin                     :: Cmp a -> Tree a -> (Maybe a, Tree a)
extractMin cmp (Bin _ Tip x r) = (Just x, insertT cmp x r)
extractMin cmp (Bin _ l   x r) = fmap (\l' -> join3 cmp l' x r) $ extractMin cmp l
extractMin _   t               = (Nothing,t)

join2             :: Cmp a -> Tree a -> Tree a -> Tree a
join2 _   Tip r   = r
join2 _   l   Tip = l
join2 cmp l   r   = let (Just m, r') = extractMin cmp r in join3 cmp l m r'

deleteT        :: Cmp a -> a -> Tree a -> Tree a
deleteT cmp x t = let (l,_,r) = splitT cmp x t in join2 cmp l r



-- | Foldmap but traversing the tree from right to left instead of left to right
reverseFoldMap                 :: Monoid m => (a -> m) -> Tree a -> m
reverseFoldMap _ Tip           = mempty
reverseFoldMap f (Bin _ l x r) = reverseFoldMap f r <> f x <> reverseFoldMap f l


size                 :: Tree a -> Size
size (Bin s _ _ _) = s
size _             = 0

----------------------------------------

delta, gamma :: Int
delta = 3
gamma = 2


isBalanced :: Tree a -> Tree b -> Bool
isBalanced l r = let sl = size l
                     sr = size r
                 in sl + sr <= 1 || delta * sl >= sr

isSingle     :: Tree a -> Tree b -> Bool
isSingle l r = size l < gamma * size r

----------------------------------------
-- | Smart constructors

bin       :: Tree a -> a -> Tree a -> Tree a
bin l x r = Bin (1 + size l + size r) l x r




----------------------------------------
-- | Rebalancing

balanceL :: Tree a -> a -> Tree a -> Tree a
balanceL l x r
  | isBalanced l r = bin l x r
  | otherwise      = rotateL l x r

balanceR :: Tree a -> a -> Tree a -> Tree a
balanceR l x r
  | isBalanced r l = bin l x r
  | otherwise      = rotateR l x r

rotateL :: Tree a -> a -> Tree a -> Tree a
rotateL l x r@(Bin _ rl _ rr)
  | isSingle rl rr = singleL l x r
  | otherwise      = doubleL l x r

rotateR :: Tree a -> a -> Tree a -> Tree a
rotateR l@(Bin _ ll _ lr) y r
  | isSingle lr ll = singleR l y r
  | otherwise      = doubleR l y r

singleL :: Tree a -> a -> Tree a -> Tree a
singleL l x (Bin _ rl y rr) = bin (bin l x rl) y rr

singleR :: Tree a -> a -> Tree a -> Tree a
singleR (Bin _ ll x lr) y r = bin ll x (bin lr y r)

doubleL :: Tree a -> a -> Tree a -> Tree a
doubleL l x (Bin _ (Bin _ rl y rr) z r) = bin (bin l x rl) y (bin rr z r)

doubleR :: Tree a -> a -> Tree a -> Tree a
doubleR (Bin _ l x (Bin _ ll y lr)) z r = bin (bin l x ll) y (bin lr z r)


--------------------------------------------------------------------------------
