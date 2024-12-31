--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Trie
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Trie type
--
--------------------------------------------------------------------------------
module HGeometry.Trie
  ( TrieF(..)
  , root
  , mapWithEdgeLabels
  , foldWithEdgeLabels

  , BinaryTrie
  , pattern Leaf, pattern OneNode, pattern TwoNode
  , asBinaryTrie

  , AtMostTwo(..)
  ) where

import           Control.Lens
import           Data.Bifoldable
import           Data.Bitraversable
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply ((<.*>))
import           Data.Functor.Classes
import           Data.Semigroup.Traversable
import           HGeometry.Sequence.KV

-- import Debug.Trace
--------------------------------------------------------------------------------

-- | The Trie data type, parameterized by the data structure storing the children.
data TrieF f e v = Node v (f e (TrieF f e v))

--  | Access the root of the trie
root :: Lens' (TrieF f e v) v
root = lens (\(Node x _) -> x) (\(Node _ chs) x -> Node x chs)
{-# INLINE root #-}


deriving instance (Show v, Show e, Show2 f) => Show (TrieF f e v)
deriving instance (Eq v, Eq e, Eq2 f)       => Eq (TrieF f e v)
deriving instance (Ord v, Ord e, Ord2 f)    => Ord (TrieF f e v)

deriving instance (Functor (f e))     => Functor (TrieF f e)
deriving instance (Foldable (f e))    => Foldable (TrieF f e)
deriving instance (Traversable (f e)) => Traversable (TrieF f e)

-- instance Foldable (f e) => Foldable1 (TrieF f e) where
--   foldMap1 f (Node v chs) = let Endo g = foldMap (\x -> Endo $ \x0 -> x0 <> foldMap1 f x) chs
--                             in g (f v)
-- somehow the order is wrong here...

instance Foldable (f e) => Foldable1 (TrieF f e) where
  foldMap1 f = go
    where
      go (Node v chs) = case foldMap (Just . go) chs of
                          Nothing -> f v
                          Just s  -> f v <> s

  -- f (Node v chs) = let Endo g = foldMap (\x -> Endo $ \x0 -> x0 <> foldMap1 f x) chs
  --                           in g (f v)


instance Traversable (f e) => Traversable1 (TrieF f e) where
  traverse1 f = go
    where
      go (Node v chs) = Node <$> f v <.*> traverse1Maybe go chs

instance Bifunctor f => Bifunctor (TrieF f) where
  bimap f g = go
    where
      go (Node v chs) = Node (g v) (bimap f go chs)

instance Bifoldable f => Bifoldable (TrieF f) where
  bifoldMap f g = go
    where
      go (Node v chs) = g v <> bifoldMap f go chs

instance Bitraversable f => Bitraversable (TrieF f) where
  bitraverse f g = go
    where
      go (Node v chs) = Node <$> g v <*> bitraverse f go chs



-- | A mapping function to transform the values of the trie, possibly using the edge
-- labels.  note that the root does not have an edge label, so it is transformed using a
-- separate function.
mapWithEdgeLabels                          :: FunctorWithIndex e (f e)
                                           => (v -> v')
                                           -- ^ function by which to transform the root
                                           -> (e -> v -> v')
                                           -> TrieF f e v -> TrieF f e v'
mapWithEdgeLabels fRoot f (Node root' chs) = Node (fRoot root') (imap go chs)
  where
    go e (Node x chs') = Node (f e x) (imap go chs')

-- | fold with the edge labels
foldWithEdgeLabels :: (FoldableWithIndex e (f e), Monoid m)
                   => (v -> m) -- ^ function by which to transform the root
                   -> (e -> v -> m)
                   -> TrieF f e v
                   -> m
foldWithEdgeLabels fRoot f (Node root' chs) = fRoot root' <> ifoldMap go chs
  where
    go e (Node x chs') = f e x <> ifoldMap go chs'

-- instance Functor f => FunctorWithIndex e (TrieF f e) where
--   imap f = go
--     where
--       go (Node x chs) = Node


--------------------------------------------------------------------------------

-- | At most two elements
data AtMostTwo a = Zero | One !a | Two !a !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Show1 AtMostTwo where
  liftShowsPrec sp _ d = \case
    Zero ->
      \s -> s <> "Zero"
    One x ->
      showsUnaryWith sp "One" d x
    Two x y ->
      showsBinaryWith sp sp "Two" d x y

instance Eq1   AtMostTwo where
  liftEq _ Zero      Zero        = True
  liftEq f (One x)   (One x')    = f x x'
  liftEq f (Two x y) (Two x' y') = f x x' && f y y'
  liftEq _ _         _           = False

instance Ord1  AtMostTwo where
  liftCompare _ Zero      Zero        = EQ
  liftCompare _ Zero      _           = LT

  liftCompare _ (One _)   Zero        = GT
  liftCompare f (One x)   (One x')    = f x x'
  liftCompare _ (One _)   (Two _ _)   = LT

  liftCompare f (Two x y) (Two x' y') = f x x' <> f y y'
  liftCompare _ _         _           = GT



-- | A binary Trie type
type BinaryTrie e v = TrieF (KV AtMostTwo) e v

-- | Pattern match on a leaf node
pattern Leaf   :: v -> BinaryTrie e v
pattern Leaf v = Node v (KV Zero)

-- | Pattern match on a node with one child
pattern OneNode     :: v -> (e, BinaryTrie e v) -> BinaryTrie e v
pattern OneNode v x = Node v (KV (One x))

-- | Pattern match on a node with two children
pattern TwoNode      :: v -> (e, BinaryTrie e v) -> (e, BinaryTrie e v) -> BinaryTrie e v
pattern TwoNode v l r = Node v (KV (Two l r))

{-# COMPLETE Leaf, OneNode, TwoNode #-}


-- | Trie to convert the trie into a binary trie.
asBinaryTrie              :: Traversable f => TrieF (KV f) e v -> Maybe (BinaryTrie e v)
asBinaryTrie (Node x chs) = traverse asBinaryTrie chs >>= \res -> case F.toList (assocs res) of
                              []    -> pure $ Leaf x
                              [c]   -> pure $ OneNode x c
                              [l,r] -> pure $ TwoNode x l r
                              _     -> Nothing
