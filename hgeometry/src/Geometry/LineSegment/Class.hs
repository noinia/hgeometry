{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.LineSegment.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class representing line segments
--
--------------------------------------------------------------------------------
module Geometry.LineSegment.Class
  ( LineSegment_(..), pattern LineSegment_

  , interpolate
  ) where

import Geometry.Interval.Class
import Geometry.Interval.Boxed
import Geometry.Interval.EndPoint
import Control.Arrow ((&&&))
import Control.Lens
import Data.Kind
import Data.Tuple (swap)
import GHC.TypeNats
import Geometry.Boundary
import Geometry.Point.Class
import Geometry.Properties
import           Geometry.Vector
-- import Data.Range (EndPoint(..))

--------------------------------------------------------------------------------

class ( HasStart (lineSegment d point r) (point d r)
      , HasEnd   (lineSegment d point r) (point d r)
      , Point_ point d r
      ) => LineSegment_ lineSegment d point r where
  {-# MINIMAL uncheckedLineSegment-}

  -- | Create a segment
  --
  -- pre: the points are disjoint
  uncheckedLineSegment :: point d r -> point d r -> lineSegment d point r

  -- | smart constructor that creates a valid segment, i.e. it
  -- validates that the endpoints are disjoint.
  mkLineSegment :: point d r -> point d r -> Maybe (lineSegment d point r)


-- | Constructs a line segment from the start and end point
pattern LineSegment_ :: forall lineSegment d point r. LineSegment_ lineSegment d point r
                     => point d r -> point d r
                     -> lineSegment d point r
pattern LineSegment_ s t <- (startAndEnd -> (s,t))
  where
    LineSegment_ s t = uncheckedLineSegment s t

--------------------------------------------------------------------------------

-- | Linearly interpolate the two endpoints with a value in the range [0,1]
--
-- >>> interpolate 0.5 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 5.0 5.0
-- >>> interpolate 0.1 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 1.0 1.0
-- >>> interpolate 0 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 0.0 0.0
-- >>> interpolate 1 $ ClosedLineSegment origin (Point2 10.0 10.0)
-- Point2 10.0 10.0
interpolate       :: forall lineSegment d point r
                     . (Fractional r, LineSegment_ lineSegment d point r)
                  => r -> lineSegment d point r -> point d r
interpolate lam (LineSegment_ s t) =
  fromVector $ (s^.asVector ^* (1-lam)) ^+^ (t^.asVector ^* lam)

--------------------------------------------------------------------------------
