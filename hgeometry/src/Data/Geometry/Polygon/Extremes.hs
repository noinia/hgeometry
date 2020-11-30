{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon.Extremes
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Finding the Extremal vertex of a polygon in a given direction.
--
--------------------------------------------------------------------------------
module Data.Geometry.Polygon.Extremes
    ( cmpExtreme
    , cmpExtreme'
    , extremesLinear
    , extremesLinearSeq
    ) where

import           Control.Lens               hiding (Simple)
import           Data.CircularSeq
import           Data.Ext
import qualified Data.Foldable              as F
import           Data.Function
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpExtreme       :: (Num r, Ord r)
                 => Vector 2 r -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cmpExtreme u p q = cmpExtreme' u (p^.core) (q^.core)

cmpExtreme'       :: (Num r, Ord r)
                 => Vector 2 r -> Point 2 r -> Point 2 r -> Ordering
cmpExtreme' u p q = u `dot` (p .-. q) `compare` 0

-- FIXME: use extremesLinearSeq in extremesLinear? Will the performance be the same?
-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- running time: \(O(n)\)
extremesLinear     :: (Ord r, Num r) => Vector 2 r -> Polygon t p r
                   -> (Point 2 r :+ p, Point 2 r :+ p)
extremesLinear u p = let vs = p^.outerBoundary
                         f  = cmpExtreme u
                     in (F.minimumBy f vs, F.maximumBy f vs)

-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- running time: \(O(n)\)
extremesLinearSeq     :: (Ord r, Num r) => Vector 2 r -> Polygon t p r
                   -> (CSeq (Point 2 r :+ p), CSeq (Point 2 r :+ p))
extremesLinearSeq u p =
    let vs = allRotations (p^.outerBoundary)
        f  = cmpExtreme u `on` focus
    in (F.minimumBy f vs, F.maximumBy f vs)
