{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.Sweep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper types and functions for implementing Sweep line algorithms.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.Sweep where

import           Data.Coerce
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Reflection
import           Unsafe.Coerce

import Data.Ext
import Data.Geometry.LineSegment
import Data.Geometry.Point

--------------------------------------------------------------------------------


newtype Safe a s = Safe a

newtype Timed t a = Timed (t -> a)


instance (Ord a, Reifies s t) => Ord (Safe (Timed t a) s) where
  p@(Safe (Timed l)) `compare` (Safe (Timed r)) = let t = reflect p
                                                  in l t `compare` r t

type StatusStructure p r = Map (Timed r r)
                               (LineSegment 2 p r)


-- wrap


build :: ( Fractional r, Ord r
         , Reifies s r
         ) => Map (Timed r r) (LineSegment 2 () r)
build = reify 0 $ \ps -> Map.fromList [ (xAt s1, s1)
                                      , (xAt s2, s2)
                                      ]
  where
    xAt s = Timed $ flip xCoordAt s

    s1 = ClosedLineSegment (ext $ Point2 0 0)  (ext $ Point2 100 0    )
    s2 = ClosedLineSegment (ext $ Point2 0 10) (ext $ Point2 100 (-10))



-- | Given a y coord and a line segment that intersects the horizontal line
-- through y, compute the x-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
xCoordAt             :: (Fractional r, Ord r) => r -> LineSegment 2 p r -> r
xCoordAt y (LineSegment' (Point2 px py :+ _) (Point2 qx qy :+ _))
      | py == qy     = px `max` qx  -- s is horizontal, and since it by the
                                    -- precondition it intersects the sweep
                                    -- line, we return the x-coord of the
                                    -- rightmost endpoint.
      | otherwise    = px + alpha * (qx - px)
  where
    alpha = (y - py) / (qy - py)
