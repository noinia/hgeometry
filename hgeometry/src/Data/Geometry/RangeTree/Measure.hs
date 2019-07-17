module Data.Geometry.RangeTree.Measure where

import           Data.BinaryTree(Measured(..))

--------------------------------------------------------------------------------

class LabeledMeasure v where
  labeledMeasure :: [a] -> v a

--------------------------------------------------------------------------------

newtype Report p = Report { reportList :: [p] }
  deriving (Show,Eq,Ord,Functor,Foldable,Semigroup,Monoid)

instance Measured (Report p) (Report p) where
  measure = id

instance LabeledMeasure Report where
  labeledMeasure = Report

--------------------------------------------------------------------------------

newtype Count a = Count { getCount :: Int } deriving (Show,Read,Eq,Ord)

instance LabeledMeasure Count where
  labeledMeasure = Count . length

instance Monoid (Count a) where
  mempty = Count 0

instance Semigroup (Count a) where
  (Count l) <> (Count r) = Count $ l + r

--------------------------------------------------------------------------------

data And l r a = And (l a) (r a) deriving (Show,Eq,Ord)

type (:*:) l r = And l r

instance (LabeledMeasure l, LabeledMeasure r) => LabeledMeasure (l :*: r) where
  labeledMeasure xs = And (labeledMeasure xs) (labeledMeasure xs)

instance (Semigroup (l a), Semigroup (r a)) => Semigroup ((l :*: r) a) where
  (And l r) <> (And l' r') = And (l <> l') (r <> r')


-- newtype All (ls :: [* -> *]) a = All (Map ls a)

-- type family Map (ls :: [* -> *]) (a :: *) where
--   Map '[]
