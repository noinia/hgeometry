--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.StringSearch.KMP
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of Knuth-Morris-Pratt String-searching
-- algorithm. The exposition is based on that of Goodrich and
-- Tamassia in "Data Structures and Algorithms in Java 2nd Edition".
--
--------------------------------------------------------------------------------
module Algorithms.StringSearch.KMP( isSubStringOf
                                  , kmpMatch
                                  , buildFailureFunction
                                  ) where

import           Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector.Generic ((!))
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder


--------------------------------------------------------------------------------

-- | Constructs the failure function.
--
-- running time: \(O(m)\).
buildFailureFunction   :: forall a. Eq a => V.Vector a -> UV.Vector Int
buildFailureFunction p = UV.create $ do
                           f <- UMV.new m
                           go f 1 0
   where
     m = V.length p
     go                        :: UMV.MVector s Int -> Int -> Int -> ST s (UMV.MVector s Int)
     go f i j | i == m         = pure f
              | p ! j == p ! i = UMV.write f i (j+1) >>  go f (i+1) (j+1)
              | j > 0          = UMV.read  f (j-1)   >>= go f i
              | otherwise      = UMV.write f i 0     >>  go f (i+1) 0

-- | Test if the first argument, the pattern p, occurs as a consecutive subsequence in t.
--
-- running time: \(O(n+m)\), where p has length \(m\) and t has length \(n\).
isSubStringOf       :: (Eq a, Foldable p, Foldable t) => p a -> t a -> Maybe Int
p `isSubStringOf` t = kmpMatch (Builder.build . Builder.foldable $ p)
                               (Builder.build . Builder.foldable $ t)


-- | Test if the first argument, the pattern p, occurs as a consecutive subsequence in t.
--
-- running time: \(O(n+m)\), where p has length \(m\) and t has length \(n\).
kmpMatch                 :: Eq a => V.Vector a -> V.Vector a -> Maybe Int
kmpMatch p t | m == 0    = Just 0
             | otherwise = kmp 0 0
  where
    m = V.length p
    n = V.length t
    f = buildFailureFunction p

    kmp i j | i == n         = Nothing
            | p ! j == t ! i = if j == m - 1 then Just (i - m + 1)
                                             else kmp (i+1) (j+1)
            | j > 0          = kmp i     (f ! (j - 1))
            | otherwise      = kmp (i+1) 0           -- j == 0
