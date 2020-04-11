{-# Language FunctionalDependencies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.BinaryTree.Weightbalanced
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--  Weight balanced binary trees, similar to those in Data.Set, that
--  store their elements in the leaves. Internal nodes correspond to
--  sets of elements, and may store those "canonical subsets" in
--  associated data structures.
--
--------------------------------------------------------------------------------
module Data.BinaryTree.WeightBalanced where


import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Apply
import Data.Bitraversable
import Data.List.NonEmpty (NonEmpty)
import Data.Measured
import Data.Semigroup.Foldable
import Data.Traversable (fmapDefault, foldMapDefault)

--------------------------------------------------------------------------------



type Size = Word

-- | Binary tree that stores its values (of type a) in the leaves. Internal
-- nodes store something of type v.
--
-- Note that this tree type is non-empty
data Tree v a = Leaf !a
              | Node (Tree v a) {-# UNPACK #-}!Size !v (Tree v a)
              deriving (Show,Eq,Ord)

size :: Tree v a -> Size
size = \case
  Leaf _       -> 1
  Node _ s _ _ -> s

-- | smart constructor that computes the size appropriately.
node       :: Tree v a -> v -> Tree v a -> Tree v a
node l v r = Node l (size l + size r) v r



instance Functor (Tree v) where
  fmap = fmapDefault
instance Foldable (Tree v) where
  foldMap = foldMapDefault
  length = \case
    Leaf _       -> 1
    Node _ s _ _ -> fromIntegral s
  minimum = minimum'
  maximum = maximum'

minimum' :: Tree v a -> a
minimum' = \case
    Leaf y       -> y
    Node l _ _ _ -> minimum' l

maximum' :: Tree v a -> a
maximum' = \case
    Leaf y       -> y
    Node _ _ _ r -> maximum' r


instance Foldable1 (Tree v)

instance Traversable (Tree v) where
  traverse f = \case
    Leaf x       -> Leaf <$> f x
    Node l s v r -> (\l' r' -> Node l' s v r') <$> traverse f l <*> traverse f r

instance Bifunctor Tree where
  bimap = bimapDefault
instance Bifoldable Tree where
  bifoldMap = bifoldMapDefault
instance Bitraversable Tree where
  -- ^ the traversal is post order, in the sense that the 'v' value
  -- stored at an internal node is evaluated only *after* traversing
  -- the children.
  bitraverse f g = \case
    Leaf x       -> Leaf <$> g x
    Node l s v r -> (\l' r' v' -> Node l' s v' r') <$> bitraverse f g l
                                                   <*> bitraverse f g r
                                                   <*> f v

-- | Fold with a tree
foldTree :: (a -> b) -> (b -> Size -> v -> b -> b) -> Tree v a -> b
foldTree f g = go
  where
    go = \case
      Leaf x       -> f x
      Node l s v r -> g (go l) s v (go r)


--------------------------------------------------------------------------------

class CanRoute v a | a -> v where
  goLeft :: a -> v -> Bool
  goLeft x = not . goLeft x
  goRight :: a -> v -> Bool
  goRight x = not . goLeft x
  {-# MINIMAL goLeft | goRight #-}

--------------------------------------------------------------------------------
-- * Queries

lookup    :: (CanRoute v a, Eq a) => a -> Tree v a -> Maybe a
lookup x = go
  where
    go = \case
      Leaf y | x == y           -> Just y
             | otherwise        -> Nothing
      Node l _ v r | goLeft x v -> go l
                   | otherwise  -> go r


minView :: (CanRoute v a, CanDelete v a) => Tree v a -> (a, Maybe (Tree v a))
minView = \case
  Leaf y       -> (y, Nothing)
  Node l _ v r -> let (m,ml) = minView l
                      mv     = deleteA m v
                  in (m, (\l' v' -> balance l' v' r) <$> ml <*> mv)

maxView :: (CanRoute v a, CanDelete v a) => Tree v a -> (a, Maybe (Tree v a))
maxView = \case
  Leaf y       -> (y, Nothing)
  Node l _ v r -> let (m,mr) = maxView r
                      mv     = deleteA m v
                  in (m, (\r' v' -> balance l v' r') <$> mr <*> mv)

--------------------------------------------------------------------------------
-- * Insertions

-- |
insert   :: (CanRoute v a, Measured v a, CanInsert v a) => a -> Tree v a -> Tree v a
insert x = go
  where
    lfx = Leaf x
    go  = \case
      lfy@(Leaf y) | goLeft x vy -> node lfx (measure x <> vy) lfy
                   | otherwise   -> node lfy (vy <> measure x) lfx
        where
          vy = measure y
      Node l _ v r | goLeft x v -> balance (go l) (insertA x v) r
                   | otherwise  -> balance l      (insertA x v) (go r)


--------------------------------------------------------------------------------
-- * Deletions


-- | Returns a maybe since the tree may now be empty
delete   :: (Eq a, CanRoute v a, Measured v a, CanDelete v a) => a -> Tree v a
         -> Maybe (Tree v a)
delete x = go
  where
    go = \case
      lfy@(Leaf y) | x == y     -> Nothing
                   | otherwise  -> Just lfy
      Node l _ v r | goLeft x v -> (\l' v' -> balance l' v' r) <$> go l <*> deleteA x v
                   | otherwise  -> (\r' v' -> balance l v' r') <$> go r <*> deleteA x v






--------------------------------------------------------------------------------
--
omega :: Word
omega = 2

delta :: Word
delta = 3

alpha :: Word
alpha = 2



insertMin   :: CanInsert v a => a -> Tree v a -> Tree v a
insertMin x = go
  where
    go = \case
      lfy@(Leaf y) -> node (Leaf x) (measure x <> measure y) lfy
      Node l _ v r -> node (go l) (insertA x v) r

insertMax   :: CanInsert v a => a -> Tree v a -> Tree v a
insertMax x = go
  where
    go = \case
      lfy@(Leaf y) -> node lfy (measure y <> measure x) (Leaf x)
      Node l _ v r -> node (go l) (insertA x v) r


join                               :: CanInsert v a => Tree v a -> Tree v a -> Tree v a
join (Leaf x)             r        = insertMin x r
join l                    (Leaf x) = insertMax x l
join l@(Node ll _ lv lr) r@(Node rl _ rv rr)
    | isUnBalanced l r = balance ll          (lv <> rv) (join lr r)
    | isUnBalanced r l = balance (join l rl) (lv <> rv) rr
    | otherwise        = node    l           (lv <> rv) r


-- | is the size of a too large w.r.t of b
isUnBalanced     :: Tree v a -> Tree v a -> Bool
isUnBalanced a b = size a > omega * size b + delta

balance       :: Tree v a -> v -> Tree v a -> Tree v a
balance l v r | isUnBalanced r l = case r of
                  Node rl _ _ rr | isSingle rl rr -> singleL l v r
                                 | otherwise      -> doubleL l v r
                  Leaf _                          -> error "balance: absurd"
                    -- since r is too big it canot be a leaf
              | isUnBalanced l r = case l of
                  Node ll _ _ lr | isSingle lr ll -> singleR l v r
                                 | otherwise      -> doubleR l v r
                  Leaf _                          -> error "balance: absurd"
              | otherwise                       = node l v r

-- | should we do a single rotation, i.e. when the size of a is too
-- small w.r.t to the size of b.
isSingle     :: Tree v a -> Tree v a -> Bool
isSingle a b = size a < alpha*size b


singleL l v r = undefined
singleR l v r = undefined

doubleL l v r = undefined
doubleR l v r = undefined


--------------------------------------------------------------------------------
