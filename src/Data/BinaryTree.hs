{-# Language DeriveFunctor #-}
module Data.BinaryTree where

import Control.Applicative
import Data.Foldable
import Data.List.NonEmpty(NonEmpty)
import Data.Semigroup
import Data.Traversable

data BinLeafTree a = Leaf a
                   | Node (BinLeafTree a) (BinLeafTree a)
                   deriving (Show,Read,Eq,Ord,Functor)

instance Foldable BinLeafTree where
  foldMap f (Leaf a) = f a
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Traversable BinLeafTree where
  traverse f (Leaf a)   = Leaf <$> f a
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

instance Semigroup (BinLeafTree a) where
  l <> r = Node l r

asBalancedBinLeafTree    :: NonEmpty a -> BinLeafTree a
asBalancedBinLeafTree ys = asBLT (length ys') ys'
  where
    ys' = toList ys

    asBLT _ [x] = Leaf x
    asBLT n xs  = let h       = n `div` 2
                      (ls,rs) = splitAt h xs
                  in Node (asBLT h ls) (asBLT (n-h) rs)
