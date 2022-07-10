{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LowerEnvelope.DualCH
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.LowerEnvelope.DualCH where

import Data.Maybe(fromJust)
import Control.Lens((^.))
import Data.Ext
import Geometry.Point
import Geometry.Point.WithExtra
import Geometry.Line
import Data.Intersection
import Algorithms.Geometry.ConvexHull.GrahamScan
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Geometry.Duality
import Data.Vinyl.CoRec

--------------------------------------------------------------------------------

type Envelope a r = NonEmpty (Line 2 r :+ a)

-- | Given a list of non-vertical lines, computes the lower envelope using
-- duality. The lines are given in left to right order.
--
-- \(O(n\log n)\)
lowerEnvelope :: (Ord r, Fractional r) => NonEmpty (Line 2 r :+ a) -> Envelope a r
lowerEnvelope = NonEmpty.reverse . lowerEnvelopeWith upperHull

type UpperHullAlgorithm point r = NonEmpty (point 2 r) -> NonEmpty (point 2 r)

-- | Given a list of non-vertical lines, computes the lower envelope by computing
-- the upper convex hull. It uses the given algorithm to do so
--
-- running time: O(time required by the given upper hull algorithm)
lowerEnvelopeWith        :: ( Fractional r, Ord r)
                         => UpperHullAlgorithm (WithExtra Point (Line 2 r :+ a)) r
                         -> NonEmpty (Line 2 r :+ a) -> Envelope a r
lowerEnvelopeWith chAlgo = fromPts . chAlgo . toPts
  where
    toPts   = fmap (\l -> WithExtra $ dualPoint' (l^.core) :+ l)
    fromPts = fmap (^._WithExtra.extra)

-- | Computes the vertices of the envelope, in left to right order
vertices   :: (Ord r, Fractional r) => Envelope a r -> [Point 2 r :+ (a,a)]
vertices e = zipWith intersect' (NonEmpty.toList e) (NonEmpty.tail e)


-- | Given two non-parallel lines, compute the intersection point and
-- return the pair of a's associated with the lines
intersect'                     :: forall r a. (Ord r, Fractional r)
                               => Line 2 r :+ a -> Line 2 r :+ a -> Point 2 r :+ (a,a)
intersect' (l :+ le) (r :+ re) = (:+ (le,re)) . fromJust
                               . asA @(Point 2 r) $ l `intersect` r
