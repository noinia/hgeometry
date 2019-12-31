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
module Algorithms.Geometry.ClosestPair.Naive( closestPair
                                            , closestPairWith
                                            , DistanceFunction
                                            ) where

import           Control.Lens ((^.),_1)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Properties (NumType)
import           Data.Geometry.Vector (Arity)
import           Data.LSeq (LSeq)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Util

--------------------------------------------------------------------------------

-- | Naive algorithm to compute the closest pair according to the
-- (squared) Euclidean distance in \(d\) dimensions. Note that we need
-- at least two elements for there to be a closest pair.
--
-- running time: \(O(dn^2)\) time.
closestPair :: ( Ord r, Arity d, Num r)
            => LSeq 2 (Point d r :+ p) -> Two (Point d r :+ p)
closestPair = (^._1) . closestPairWith (\p q -> squaredEuclideanDist (p^.core) (q^.core))


type DistanceFunction g = g -> g -> NumType g

-- | Naive algorithm to compute the closest pair of points (and the
-- distance realized by those points) given a distance function.  Note
-- that we need at least two elements for there to be a closest pair.
--
-- running time: \(O(T(d)n^2)\), where \(T(d)\) is the time required
-- to evaluate the distance between two points in \(\mathbb{R}^d\).
closestPairWith   :: Ord r
                  => DistanceFunction (Point d r :+ p)
                  -> LSeq 2 (Point d r :+ p) -> SP (Two (Point d r :+ p)) r
closestPairWith d = getVal . getMin . sconcat . fmap mkPair . pairs
  where
    getVal (Arg dist x) = SP x dist
    mkPair (Two p q)    = Min (Arg (d p q) (Two p q))

-- | Produce all lists from a vec of elements. Since the Vec contains at least two
-- elements, the resulting list is non-empty
pairs :: LSeq 2 a -> NonEmpty.NonEmpty (Two a)
pairs = NonEmpty.fromList . uniquePairs . F.toList
