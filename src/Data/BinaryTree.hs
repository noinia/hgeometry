{-# Language DeriveFunctor #-}
{-# Language FunctionalDependencies #-}
module Data.BinaryTree where

import Control.Applicative
import Data.Foldable
import Data.List.NonEmpty(NonEmpty)
import Data.Semigroup
import Data.Traversable
import Data.Semigroup.Foldable

data BinLeafTree v a = Leaf a
                     | Node (BinLeafTree v a) v (BinLeafTree v a)
                     deriving (Show,Read,Eq,Ord,Functor)


class Semigroup v => Measured v a | a -> v where
  measure :: a -> v

-- | smart constructor
node     :: Measured v a => BinLeafTree v a -> BinLeafTree v a -> BinLeafTree v a
node l r = Node l (measure l <> measure r) r


instance Measured v a => Measured v (BinLeafTree v a) where
  measure (Leaf x)     = measure x
  measure (Node _ v _) = v


instance Foldable (BinLeafTree v) where
  foldMap f (Leaf a)     = f a
  foldMap f (Node l _ r) = foldMap f l `mappend` foldMap f r

instance Foldable1 (BinLeafTree v)

instance Traversable (BinLeafTree v) where
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l v r) = Node <$> traverse f l <*> pure v <*> traverse f r

instance Measured v a => Semigroup (BinLeafTree v a) where
  l <> r = node l r


asBalancedBinLeafTree    :: NonEmpty a -> BinLeafTree Size (Elem a)
asBalancedBinLeafTree ys = asBLT (length ys') ys'
  where
    ys' = toList ys

    asBLT _ [x] = Leaf (Elem x)
    asBLT n xs  = let h       = n `div` 2
                      (ls,rs) = splitAt h xs
                  in node (asBLT h ls) (asBLT (n-h) rs)

newtype Size = Size Int deriving (Show,Read,Eq,Num,Integral,Enum,Real,Ord)

instance Semigroup Size where
  x <> y = x + y

newtype Elem a = Elem { _unElem :: a }
               deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Measured Size (Elem a) where
  measure _ = 1
