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
module Data.Geometry.Polygon.Extremes( cmpExtreme
                                     , extremesLinear
                                     ) where

import           Control.Lens hiding (Simple)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpExtreme       :: (Num r, Ord r)
                 => Vector 2 r -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cmpExtreme u p q = u `dot` (p^.core .-. q^.core) `compare` 0


-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- running time: \(O(n)\)
extremesLinear     :: (Ord r, Num r) => Vector 2 r -> Polygon t p r
                   -> (Point 2 r :+ p, Point 2 r :+ p)
extremesLinear u p = let vs = p^.outerBoundary
                         f  = cmpExtreme u
                     in (F.minimumBy f vs, F.maximumBy f vs)
