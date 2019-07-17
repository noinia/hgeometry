module Data.Geometry.RangeTree.Measure where

import qualified Data.Geometry.RangeTree.Generic as GRT

class MeasuredRT v where
  measureRT :: [a] -> v a


--------------------------------------------------------------------------------
instance MeasuredRT GRT.Report where
  measureRT = GRT.Report
--------------------------------------------------------------------------------

newtype Count a = Count { getCount :: Int } deriving (Show,Read,Eq,Ord)

instance MeasuredRT Count where
  measureRT = Count . length

instance Monoid (Count a) where
  mempty = Count 0

instance Semigroup (Count a) where
  (Count l) <> (Count r) = Count $ l + r

--------------------------------------------------------------------------------

data And l r a = And (l a) (r a) deriving (Show,Eq,Ord)

type (:*:) l r = And l r

instance (MeasuredRT l, MeasuredRT r) => MeasuredRT (l :*: r) where
  measureRT xs = And (measureRT xs) (measureRT xs)

instance (Semigroup (l a), Semigroup (r a)) => Semigroup ((l :*: r) a) where
  (And l r) <> (And l' r') = And (l <> l') (r <> r')


-- newtype All (ls :: [* -> *]) a = All (Map ls a)

-- type family Map (ls :: [* -> *]) (a :: *) where
--   Map '[]
