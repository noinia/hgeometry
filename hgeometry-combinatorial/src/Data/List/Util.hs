module Data.List.Util where

import qualified Data.List as List
import           Data.Maybe

-- | Simple Zipper for Lists.
data Zipper a = Zipper [a] [a] deriving (Show,Eq,Functor)

-- | Construct a Zipper from a list
--
-- running time: \(O(1)\)
fromList :: [a] -> Zipper a
fromList = Zipper []

-- | Constructs a list out of the current zipper
--
-- running time: \(O(n)\)
toList                :: Zipper a -> [a]
toList (Zipper xs ys) = reverse xs <> ys

-- | Go to the Next Element
--
-- running time: \(O(1)\)
goNext                :: Zipper a -> Maybe (Zipper a)
goNext (Zipper xs ys) = case ys of
                          []    -> Nothing
                          x:ys' -> Just $ Zipper (x:xs) ys'

-- | Go to the previous Element
--
-- running time: \(O(1)\)
goPrev                :: Zipper a -> Maybe (Zipper a)
goPrev (Zipper xs ys) = case xs of
                          []    -> Nothing
                          x:xs' -> Just $ Zipper xs' (x:ys)

-- | Computes all nexts, even one that has no elements initially or at
-- the end.
--
-- >>> mapM_ print $ allNexts $ fromList [1..5]
-- Zipper [] [1,2,3,4,5]
-- Zipper [1] [2,3,4,5]
-- Zipper [2,1] [3,4,5]
-- Zipper [3,2,1] [4,5]
-- Zipper [4,3,2,1] [5]
-- Zipper [5,4,3,2,1] []
allNexts :: Zipper a -> [Zipper a]
allNexts = List.unfoldr (fmap (\z -> (z,goNext z))) . Just

-- | Drops the next element in the zipper.
--
-- running time: \(O(1)\)
dropNext                :: Zipper a -> Maybe (Zipper a)
dropNext (Zipper xs ys) = case ys of
                            []      -> Nothing
                            (_:ys') -> Just $ Zipper xs ys'

-- | Computes all list that still have next elements.
--
-- >>> mapM_ print $ allNexts' $ fromList [1..5]
-- Zipper [] [1,2,3,4,5]
-- Zipper [1] [2,3,4,5]
-- Zipper [2,1] [3,4,5]
-- Zipper [3,2,1] [4,5]
-- Zipper [4,3,2,1] [5]
allNonEmptyNexts :: Zipper a -> [Zipper a]
allNonEmptyNexts = List.unfoldr (\z -> (z,) <$> goNext z)

-- | Given an input list, computes all lists in which just one element is missing.
--
-- >>> mapM_ print $ leaveOutOne [1..5]
-- [2,3,4,5]
-- [1,3,4,5]
-- [1,2,4,5]
-- [1,2,3,5]
-- [1,2,3,4]
-- >>> leaveOutone []
-- []
-- >>> leaveOutOne [1]
-- [[]]
leaveOutOne    :: [a] -> [[a]]
leaveOutOne xs = (toList . fromJust . dropNext) <$> allNonEmptyNexts (fromList xs)
