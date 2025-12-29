--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Foldable.Sort
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Sorting foldable collections by using fast sorting functions from
-- vector-algorithms.
--
--------------------------------------------------------------------------------
module HGeometry.Foldable.Sort
  ( sortBy
  , sort
  , sortOn
  , sortOnCheap
  ) where

import           Control.Monad.ST
import           Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector as BoxedVector
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.MVector as MVectorBuilder
import qualified VectorBuilder.Vector as VectorBuilder

--------------------------------------------------------------------------------

-- | Sort the given collection using intro sort.
--
-- \(O(n\log n)\)
sort :: forall vector f a. ( Foldable f
                           , Vector.Vector vector a
                           , Ord a
                           )
     => f a -> vector a
sort = sortBy compare
{-# INLINE sort #-}


-- | Sort the collection using the given "expensive" function; i.e. the
-- function f is called only once and cached.
--
-- \(O(Tn + n\log n)\), where \(T\) is the time required to evaluate the function
sortOn      :: forall f a b.
               (Foldable f, Ord b)
            => (a -> b) -> f a -> BoxedVector.Vector a
sortOn f = fmap (\(Helper _ x) -> x)
         . sortOnCheap @_ @BoxedVector.Vector (\(Helper b _) -> b)
         . fmap  (\x -> Helper (f x) x)
         . VectorBuilder.build . Builder.foldable
{-# INLINABLE sortOn #-}

data Helper b a = Helper !b a

-- | Sort the collection using the given "cheap" function; i.e. the
-- function f is called at every comparison
--
-- \(O(Tn\log n)\), where \(T\) is the time required to evaluate the function
sortOnCheap   :: forall vector f a b. ( Foldable f
                                 , Vector.Vector vector a
                                 , Ord b
                                 )
              => (a -> b) -> f a -> vector a
sortOnCheap f = sortBy (comparing f)
{-# INLINE sortOnCheap #-}

-- | Sort a collection using the given comparator using intro-sort (essentially quicksort).
--
-- \(O(T n\log n)\), where \(T\) is the time to do a comparison.
sortBy        :: forall vector f a. ( Foldable f
                                    , Vector.Vector vector a)
              => (a -> a -> Ordering) -> f a -> vector a
sortBy cmp xs = runST $ do v <- MVectorBuilder.build $ Builder.foldable xs
                           Intro.sortBy cmp v
                           Vector.unsafeFreeze v
{-# INLINABLE sortBy #-}
