--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ClosestPair.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive \O(n\^2)\) time algorithm to compute the closest pair of points among
-- \(n\) points in \(\mathbb{R}^d\).
--
--------------------------------------------------------------------------------
module HGeometry.ClosestPair.Naive
  ( closestPair
  , closestPairWith
  , DistanceFunction
  ) where

import           Control.Lens ((^.),_1)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           HGeometry.Combinatorial.Util
import           HGeometry.Point
import           HGeometry.Properties (NumType)
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Naive algorithm to compute the closest pair according to the
-- (squared) Euclidean distance in \(d\) dimensions. Note that we need
-- at least two elements for there to be a closest pair.
--
-- pre: there are at least two! points in the input
--
-- running time: \(O(dn^2)\) time.
closestPair :: ( Ord r, Num r, Point_ point d r, Foldable1 f, Has_ Metric_ d r)
            => f point -> Vector 2 point
closestPair = (^._1) . closestPairWith squaredEuclideanDist

-- | A distance function between geometries of type 'g'
type DistanceFunction g = g -> g -> NumType g

-- | Naive algorithm to compute the closest pair of points (and the
-- distance realized by those points) given a distance function.  Note
-- that we need at least two elements for there to be a closest pair.
--
-- running time: \(O(T(d)n^2)\), where \(T(d)\) is the time required
-- to evaluate the distance between two points in \(\mathbb{R}^d\).
closestPairWith   :: (Ord r, Point_ point d r, Foldable1 f)
                  => DistanceFunction point
                  -> f point -> SP (Vector 2 point) r
closestPairWith d = getVal . getMin . sconcat . fmap mkPair . pairs
  where
    getVal (Arg dist x) = SP x dist
    mkPair (Two p q)    = Min (Arg (d p q) (Vector2 p q))

-- | Produce all lists from a vec of elements. Since the Vec contains at least two
-- elements, the resulting list is non-empty
pairs :: Foldable1 f => f a -> NonEmpty.NonEmpty (Two a)
pairs = NonEmpty.fromList . uniquePairs . F.toList
