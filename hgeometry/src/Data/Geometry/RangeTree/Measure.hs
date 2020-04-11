module Data.Geometry.RangeTree.Measure where

import Data.Measured.Class
import Data.Functor.Product(Product(..))
import Data.Functor.Classes

--------------------------------------------------------------------------------

class LabeledMeasure v where
  labeledMeasure :: [a] -> v a

--------------------------------------------------------------------------------

newtype Report p = Report { reportList :: [p] }
  deriving (Show,Eq,Ord,Functor,Foldable,Semigroup,Monoid,Show1,Eq1)

instance Measured (Report p) (Report p) where
  measure = id

instance LabeledMeasure Report where
  labeledMeasure = Report

--------------------------------------------------------------------------------

newtype Count a = Count { getCount :: Int } deriving (Show,Read,Eq,Ord)

instance Show1 Count where
  liftShowsPrec _  _ = showsPrec
instance Eq1 Count where
  liftEq _ (Count a) (Count b) = a == b

instance LabeledMeasure Count where
  labeledMeasure = Count . length

instance Monoid (Count a) where
  mempty = Count 0

instance Semigroup (Count a) where
  (Count l) <> (Count r) = Count $ l + r

--------------------------------------------------------------------------------

type (:*:) l r = Product l r

instance (LabeledMeasure l, LabeledMeasure r) => LabeledMeasure (l :*: r) where
  labeledMeasure xs = Pair (labeledMeasure xs) (labeledMeasure xs)

instance (Semigroup (l a), Semigroup (r a)) => Semigroup ((l :*: r) a) where
  (Pair l r) <> (Pair l' r') = Pair (l <> l') (r <> r')

instance (Monoid (l a), Monoid (r a)) => Monoid ((l :*: r) a) where
  mempty = Pair mempty mempty



-- newtype All (ls :: [* -> *]) a = All (Map ls a)

-- type family Map (ls :: [* -> *]) (a :: *) where
--   Map '[]
