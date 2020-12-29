--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Set
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.List.Set( Set, singleton
                    , insert, delete
                    , union, intersection, difference
                    , fromList, insertAll
                    ) where

import qualified Data.List as List

--------------------------------------------------------------------------------

-- | A Set of 'a's, implemented using a simple list. The only
-- advantage of this implementation over 'Data.Set' from containers is
-- that most operations require only 'Eq a' rather than 'Ord a'.
newtype Set a = Set { toList :: [a] }
              deriving (Show,Read,Functor,Foldable,Traversable)

instance Eq a => Eq (Set a) where
  (Set xs) == (Set ys) = all (`elem` ys) xs &&  all (`elem` xs) ys


instance Eq a => Semigroup (Set a) where
  (Set xs) <> s = insertAll xs s

instance Eq a => Monoid (Set a) where
  mempty = Set []

-- | Creates a singleton set.
singleton   :: a -> Set a
singleton x = Set [x]

-- | \(O(n)\) Inserts an element in the set
insert                           :: Eq a => a -> Set a -> Set a
insert x s@(Set xs) | x `elem` s = s
                    | otherwise  = Set (x:xs)

-- | \( O(n^2) \) Insert an element in a set.
insertAll      :: Eq a => [a] -> Set a -> Set a
insertAll xs s = List.foldl' (flip insert) s xs

-- | \( O(n^2) \) Create a set from a finite list of elements.
fromList :: Eq a => [a] -> Set a
fromList = flip insertAll mempty

-- | \(O(n)\) Deletes an element from the set
delete            :: Eq a => a -> Set a -> Set a
delete x (Set xs) = Set $ go xs
  where
    go = \case
      [] -> []
      (y:ys) | x == y    -> ys -- found the element, no need to continue looking
             | otherwise -> y:go ys

-- | \(O(n^2)\) Computes the union of two sets
union :: Eq a => Set a -> Set a -> Set a
union = (<>)

-- | \(O(n^2)\) Computes the intersection of two sets
intersection                     :: Eq a => Set a -> Set a -> Set a
(Set xs) `intersection` (Set ys) = Set (xs `List.intersect` ys)

-- | \(O(n^2)\) Computes the difference of two sets
difference :: Eq a => Set a -> Set a -> Set a
(Set xs) `difference` (Set ys) = Set $ xs List.\\ ys
