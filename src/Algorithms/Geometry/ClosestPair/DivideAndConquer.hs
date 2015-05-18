{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
module Algorithms.Geometry.ClosestPair.DivideAndConquer where

import           Algorithms.Geometry.ClosestPair.Naive(Two(..), PP, mkPair, getVal)
import           Control.Applicative

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry(qdA)
import           Data.Geometry.Point
import           Data.Geometry.Vector(Arity)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup
import qualified Data.Traversable as Tr
import qualified Data.Vector.Fixed as FV

import qualified Data.BinaryTree as BT


-- | divide and conquer algo for 2 dimensional closest pair
-- Note that we need at least two elements
-- for there to be a closest pair.
closestPair :: ( FV.Dim v ~ FV.S (FV.S n)
               , FV.Vector v (Point 2 r :+ p)
               , Ord r, Num r
               ) => v (Point 2 r :+ p) -> Two (Point 2 r :+ p)
closestPair = undefined



newtype Count = Count Int deriving (Show,Eq,Ord,Num,Integral,Real,Enum)

instance Semigroup Count where
  a <> b = a + b

instance Monoid Count where
  mempty = 1
  a `mappend` b = a <> b

data Lab a = Lab !(Max a) !Count


data Tree v a = Leaf a
              | Node (Tree v a) v (Tree v a)
              deriving (Show,Eq)

node     :: (Measure v a, Semigroup v) => Tree v a -> Tree v a -> Tree v a
node l r = Node l (measure l <> measure r) r

class Semigroup v => Measure v a | a -> v where
  measure :: a -> v

instance Measure v a => Measure v (Tree v a) where
  measure (Leaf a)     = measure a
  measure (Node _ v _) = v

instance Measure v a => Semigroup (Tree v a) where
  (<>) = node


traverse'  :: (Measure v b, Semigroup v, Applicative f) =>
              (a -> f b) -> Tree u a -> f (Tree v b)
traverse' f (Leaf a)     = Leaf <$> f a
traverse' f (Node l _ r) = node <$> traverse' f l <*> traverse' f r


-- annotate :: BinLeafTree a -> Tree (Lab a) a
-- annotate =  traverse (\x -> Leaf $ Lab (Max x mempty))
