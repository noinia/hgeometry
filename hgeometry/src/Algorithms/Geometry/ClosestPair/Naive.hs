--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ClosestPair.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive \O(n\^2)\) time algorithm to compute the closest pair of points among
-- \(n\) points in \(\mathbb{R}^d\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ClosestPair.Naive where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry (qdA)
import           Data.Geometry.Point
import           Data.Geometry.Vector (Arity)
import           Data.LSeq (LSeq)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Util

--------------------------------------------------------------------------------

-- | Naive algorithm to compute the closest pair in \(d\) dimensions. Runs in
-- \(O(n^2)\) time (for any constant \(d\)). Note that we need at least two elements
-- for there to be a closest pair.
closestPair :: ( Ord r, Arity d, Num r
               ) => LSeq 2 (Point d r :+ p) -> Two (Point d r :+ p)
closestPair = getVal . getMin . sconcat . fmap (uncurry' mkPair) . pairs
  where
    uncurry' f (Two a b) = f a b
    getVal (Arg _ x) = x

-- | A pair of points
type PP d p r = ArgMin r (Two (Point d r :+ p))

-- | Create a pair of points
mkPair                         :: (Arity d, Num r)
                               => Point d r :+ p -> Point d r :+ p -> PP d p r
mkPair pp@(p :+ _) qq@(q :+ _) = let dst = qdA p q
                                 in Min (Arg dst (Two pp qq))

-- | Produce all lists from a vec of elements. Since the Vec contains at least two
-- elements, the resulting list is non-empty
pairs :: LSeq 2 a -> NonEmpty.NonEmpty (Two a)
pairs = NonEmpty.fromList . uniquePairs . F.toList
