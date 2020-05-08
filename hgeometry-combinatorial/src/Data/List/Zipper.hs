module Data.List.Zipper where

import qualified Data.List as List

--------------------------------------------------------------------------------

-- | Simple Zipper for Lists.
data Zipper a = Zipper [a] [a] deriving (Show,Eq,Functor)

instance Foldable Zipper where
  -- Folds like it it is a normal list
  foldMap f (Zipper ls rs) = foldMap f (reverse ls) <> foldMap f rs


-- | Construct a Zipper from a list
--
-- running time: \(O(1)\)
fromList :: [a] -> Zipper a
fromList = Zipper []

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

-- | Returns the next element, and the zipper without it
extractNext                :: Zipper a -> Maybe (a, Zipper a)
extractNext (Zipper xs ys) = case ys of
                               []      -> Nothing
                               (y:ys') -> Just $ (y,Zipper xs ys')


-- | Drops the next element in the zipper.
--
-- running time: \(O(1)\)
dropNext :: Zipper a -> Maybe (Zipper a)
dropNext = fmap snd . extractNext

-- | Computes all list that still have next elements.
--
-- >>> mapM_ print $ allNonEmptyNexts $ fromList [1..5]
-- Zipper [] [1,2,3,4,5]
-- Zipper [1] [2,3,4,5]
-- Zipper [2,1] [3,4,5]
-- Zipper [3,2,1] [4,5]
-- Zipper [4,3,2,1] [5]
allNonEmptyNexts :: Zipper a -> [Zipper a]
allNonEmptyNexts = List.unfoldr (\z -> (z,) <$> goNext z)
