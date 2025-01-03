--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Trie.Binary
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Trie type
--
--------------------------------------------------------------------------------
module HGeometry.Trie.Binary
  ( TrieF(Node)
  , BinaryTrie
  , pattern Leaf, pattern LeftNode, pattern RightNode, pattern TwoNode
  , asBinaryTrie

  , AtMostTwoOriented(..)
  ) where


import qualified Data.Foldable as F
import           Data.Functor.Classes
import           HGeometry.Sequence.KV
import           HGeometry.Trie.Type

--------------------------------------------------------------------------------

-- | At most two elements. A one element can either be left or right
data AtMostTwoOriented a = Zero | OneLeft !a | OneRight !a | Two !a !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Show1 AtMostTwoOriented where
  liftShowsPrec sp _ d = \case
    Zero ->
      \s -> s <> "Zero"
    OneLeft x ->
      showsUnaryWith sp "OneLeft" d x
    OneRight x ->
      showsUnaryWith sp "OneRight" d x
    Two x y ->
      showsBinaryWith sp sp "Two" d x y

instance Eq1   AtMostTwoOriented where
  liftEq _ Zero         Zero          = True
  liftEq f (OneLeft x)  (OneLeft x')  = f x x'
  liftEq f (OneRight x) (OneRight x') = f x x'
  liftEq f (Two x y)    (Two x' y')   = f x x' && f y y'
  liftEq _ _         _                = False

instance Ord1  AtMostTwoOriented where
  liftCompare _ Zero          Zero         = EQ
  liftCompare _ Zero          _            = LT

  liftCompare _ (OneLeft _)   Zero         = GT
  liftCompare f (OneLeft x)   (OneLeft x') = f x x'
  liftCompare _ (OneLeft _)   (OneRight _) = LT
  liftCompare _ (OneLeft _)   (Two _ _)    = LT

  liftCompare _ (OneRight _) Zero          = GT
  liftCompare _ (OneRight _) (OneLeft _)   = GT
  liftCompare f (OneRight x) (OneRight x') = f x x'
  liftCompare _ (OneRight _) (Two _ _)     = LT

  liftCompare f (Two x y)    (Two x' y')   = f x x' <> f y y'
  liftCompare _ _            _             = GT


--------------------------------------------------------------------------------

-- | A binary Trie type
type BinaryTrie e v = TrieF (KV AtMostTwoOriented) e v

-- | Pattern match on a leaf node
pattern Leaf   :: v -> BinaryTrie e v
pattern Leaf v = Node v (KV Zero)

-- | Pattern match on a node with one child, namely on the left
pattern LeftNode     :: v -> (e, BinaryTrie e v) -> BinaryTrie e v
pattern LeftNode v x = Node v (KV (OneLeft x))

-- | Pattern match on a node with one child, namely on the left
pattern RightNode     :: v -> (e, BinaryTrie e v) -> BinaryTrie e v
pattern RightNode v x = Node v (KV (OneRight x))


-- | Pattern match on a node with two children
pattern TwoNode      :: v -> (e, BinaryTrie e v) -> (e, BinaryTrie e v) -> BinaryTrie e v
pattern TwoNode v l r = Node v (KV (Two l r))

{-# COMPLETE Leaf, LeftNode, RightNode, TwoNode #-}

--------------------------------------------------------------------------------

-- | Trie to convert the trie into a binary trie. If we only have one element,
-- we alwasy produce a left node.
asBinaryTrie              :: Traversable f => TrieF (KV f) e v -> Maybe (BinaryTrie e v)
asBinaryTrie (Node x chs) = traverse asBinaryTrie chs >>= \res -> case F.toList (assocs res) of
                              []    -> pure $ Leaf x
                              [c]   -> pure $ LeftNode x c
                              [l,r] -> pure $ TwoNode x l r
                              _     -> Nothing
