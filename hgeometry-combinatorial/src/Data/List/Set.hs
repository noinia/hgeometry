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

-- | Inserts an element in the set
--
-- running time: \(O(n)\)
insert                           :: Eq a => a -> Set a -> Set a
insert x s@(Set xs) | x `elem` s = s
                    | otherwise  = Set (x:xs)

insertAll      :: Eq a => [a] -> Set a -> Set a
insertAll xs s = List.foldl' (flip insert) s xs

fromList :: Eq a => [a] -> Set a
fromList = flip insertAll mempty

-- | Deletes an element from the set
--
-- running time: \(O(n)\)
delete            :: Eq a => a -> Set a -> Set a
delete x (Set xs) = Set $ go xs
  where
    go = \case
      [] -> []
      (y:ys) | x == y    -> ys -- found the element, no need to continue looking
             | otherwise -> y:go ys

-- | Computes the union of two sets
--
-- running time: \(O(n^2)\)
union :: Eq a => Set a -> Set a -> Set a
union = (<>)

-- | Computes the intersection of two sets
--
-- running time: \(O(n^2)\)
intersection                     :: Eq a => Set a -> Set a -> Set a
(Set xs) `intersection` (Set ys) = Set (xs `List.intersect` ys)

-- | Computes the difference of two sets
--
-- running time: \(O(n^2)\)
difference :: Eq a => Set a -> Set a -> Set a
(Set xs) `difference` (Set ys) = Set $ xs List.\\ ys
