--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.List.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.List.Util where

import           Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           HGeometry.Ext
import           HGeometry.List.Zipper (allNonEmptyNexts, extractNext)
import qualified HGeometry.List.Zipper as Zipper
import qualified HGeometry.NonEmpty.Util as NonEmptyUtil

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
-- >>> minimumMaybe [] :: Maybe ()
-- Nothing
-- >>> minimumMaybe [1,2,3]
-- Just 1
minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe = minimumMaybeBy compare

-- | Safe variant of Prelude.maximum.
--
-- >>> maximumMaybe [] :: Maybe ()
-- Nothing
-- >>> maximumMaybe [1,2,3]
-- Just 3
maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe = minimumMaybeBy (flip compare)

-- | Total variant of Data.List.minimumBy.
--
-- >>> minimumMaybeBy (comparing abs) [] :: Maybe Int
-- Nothing
-- >>> minimumMaybeBy (comparing abs) [1,-2,3]
-- Just 1
minimumMaybeBy     :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumMaybeBy cmp = \case
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

-- | Extracts all minima from the list. The result consists of the
-- list of minima, and all remaining points. Both lists are returned
-- in the order in which they occur in the input.
--
-- >>> extractMinimaBy compare [1,2,3,0,1,2,3,0,1,2,0,2]
-- [0,0,0] :+ [2,3,1,2,3,1,2,1,2]
extractMinimaBy        :: (a -> a -> Ordering) -> [a] -> [a] :+ [a]
extractMinimaBy cmp xs = case NonEmpty.nonEmpty xs of
    Nothing  -> [] :+ []
    Just xs' -> first NonEmpty.toList $ NonEmptyUtil.extractMinimaBy cmp xs'
  -- (x:xs) ->  $ foldr (\y (mins@(m:|_) :+ rest) ->
  --                                            case m `cmp` y of
  --                                              LT -> mins :+ y:rest
  --                                              EQ -> (y NonEmpty.<| mins) :+ rest
  --                                              GT -> (y:|[]) :+ NonEmpty.toList mins <> rest
  --                                         ) ((x:|[]) :+ []) xs


--------------------------------------------------------------------------------
-- * Partitioning and Grouping

-- | Given a function f, partitions the list into three lists
-- (lts,eqs,gts) such that:
--
-- - f x == LT for all x in lts
-- - f x == EQ for all x in eqs
-- - f x == gt for all x in gts
--
-- >>> partition3 (compare 4) [0,1,2,2,3,4,5,5,6,6,7,7,7,7,7,8]
-- ([5,5,6,6,7,7,7,7,7,8],[4],[0,1,2,2,3])
--
partition3   :: Foldable f => (a -> Ordering) -> f a -> ([a],[a],[a])
partition3 f = foldr g ([],[],[])
  where
    g x (lts,eqs,gts) = case f x of
                          LT -> (x:lts,   eqs,  gts)
                          EQ -> (  lts, x:eqs,  gts)
                          GT -> (  lts,   eqs,x:gts)

-- | A version of groupBy that uses the given Ordering to group
-- consecutive Equal items
--
-- >>> groupBy' compare [0,1,2,2,3,4,5,5,6,6,7,7,7,7,7,8]
-- [0 :| [],1 :| [],2 :| [2],3 :| [],4 :| [],5 :| [5],6 :| [6],7 :| [7,7,7,7],8 :| []]
groupBy'     :: (a -> a -> Ordering) -> [a] -> [NonEmpty a]
groupBy' cmp = go
  where
    go = \case
      []       -> []
      (x:xs)   -> let (pref,rest) = List.span (\y -> x `cmp` y == EQ) xs
                  in (x :| pref) : go rest
