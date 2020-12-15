--------------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.List.Util where

import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Zipper (allNonEmptyNexts, extractNext)
import qualified Data.List.Zipper as Zipper
import           Data.Maybe
import           Data.Ord (comparing)

--------------------------------------------------------------------------------

-- | Given an input list, computes all lists in which just one element is missing.
--
-- >>> mapM_ print $ leaveOutOne [1..5]
-- (1,[2,3,4,5])
-- (2,[1,3,4,5])
-- (3,[1,2,4,5])
-- (4,[1,2,3,5])
-- (5,[1,2,3,4])
-- >>> leaveOutOne []
-- []
-- >>> leaveOutOne [1]
-- [(1,[])]
leaveOutOne    :: [a] -> [(a,[a])]
leaveOutOne xs = second F.toList . fromJust . extractNext
              <$> allNonEmptyNexts (Zipper.fromList xs)


--------------------------------------------------------------------------------
-- * Improved functions for minima and maxima

-- | Safe variant of Prelude.minimum.
--
-- >>> minimum1 [] :: Maybe ()
-- Nothing
-- >>> minimum1 [1,2,3]
-- Just 1
minimum1 :: Ord a => [a] -> Maybe a
minimum1 = minimum1By compare

-- | Safe variant of Prelude.maximum.
--
-- >>> maximum [] :: Maybe ()
-- Nothing
-- >>> maximum1 [1,2,3]
-- Just 3
maximum1 :: Ord a => [a] -> Maybe a
maximum1 = minimum1By (flip compare)

-- | Total variant of Data.List.minimumBy.
--
-- >>> minimum1By (comparing abs) [] :: Maybe ()
-- Nothing
-- >>> minimum1By (comparing abs) [1,-2,3]
-- Just 1
minimum1By     :: (a -> a -> Ordering) -> [a] -> Maybe a
minimum1By cmp = \case
  [] -> Nothing
  xs -> Just $ List.minimumBy cmp xs

-- | Computes all minima by comparing some property.
--
-- >>> minimaOn (max 2) [1,2,3,4,5,-1]
-- [-1,2,1]
minimaOn   :: Ord b => (a -> b) -> [a] -> [a]
minimaOn f = minimaBy (comparing f)

-- | Computes all minima.
--
-- >>> minimaBy (comparing abs) [1,2,3,2,1,-1]
-- [-1,1,1]
minimaBy     :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp = \case
  []     -> []
  (x:xs) -> NonEmpty.toList $ List.foldl' (\mins@(m:|_) y -> case m `cmp` y of
                                                               LT -> mins
                                                               EQ -> y NonEmpty.<| mins
                                                               GT -> y:|[]
                                          ) (x:|[]) xs

-- | extracts all minima from the list. The result consists of the
-- list of minima, and all remaining points. Both lists are returned
-- in the order in which they occur in the input.
--
-- >>> extractMinimaBy compare [1,2,3,0,1,2,3,0,1,2,0,2]
-- [0,0,0] :+ [2,3,1,2,3,1,2,1,2]
extractMinimaBy     :: (a -> a -> Ordering) -> [a] -> [a] :+ [a]
extractMinimaBy cmp = \case
  []     -> [] :+ []
  (x:xs) -> first NonEmpty.toList $ foldr (\y (mins@(m:|_) :+ rest) ->
                                             case m `cmp` y of
                                               LT -> mins :+ y:rest
                                               EQ -> (y NonEmpty.<| mins) :+ rest
                                               GT -> (y:|[]) :+ NonEmpty.toList mins <> rest
                                          ) ((x:|[]) :+ []) xs
  -- TODO: This is actually a good scenario for testing how much slower :+ is compared
  -- to doing nothing. i..e compare minimaBy and extractMinimaBy
  -- note that I'm using foldr here, and foldl' before
