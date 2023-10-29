--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.HalfOpen
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Half-Open Intervals
--
--------------------------------------------------------------------------------
module HGeometry.Interval.HalfOpen
  ( HalfOpenInterval(HalfOpenInterval)
  ) where


import           Control.Lens
-- import           Data.Foldable1
import           HGeometry.Interval.Class
-- import           HGeometry.Interval.EndPoint
import           HGeometry.Interval.Internal
import           HGeometry.Point
-- import HGeometry.Measured.Class
import           HGeometry.Properties
import           HGeometry.Intersection
-- import qualified Data.List.NonEmpty as NonEmpty
-- import           Data.List.NonEmpty (NonEmpty(..))


--------------------------------------------------------------------------------

-- | An interval that is open on the left and closed on the right
data HalfOpenInterval r = HalfOpenInterval { _left  :: !(EndPoint Open   r)
                                           , _right :: !(EndPoint Closed r)
                                           }
                        deriving (Show,Eq,Functor,Foldable,Traversable)

type instance NumType (HalfOpenInterval r) = r

type instance StartPointOf (HalfOpenInterval r) = EndPoint Open   r
type instance EndPointOf  (HalfOpenInterval r)  = EndPoint Closed r

instance HasStart (HalfOpenInterval r) r where
  start = startPoint._endPoint

instance HasEnd (HalfOpenInterval r) r where
  end = endPoint._endPoint

instance HasStartPoint (HalfOpenInterval r) (EndPoint Open r) where
  startPoint = lens _left (\ai x -> ai { _left = x})

instance HasEndPoint (HalfOpenInterval r) (EndPoint Closed r) where
  endPoint = lens _right (\ai x -> ai { _right = x})

instance IntervalLike_ (HalfOpenInterval r) r where
  mkInterval = HalfOpenInterval

instance Interval_ (HalfOpenInterval r) r where

instance (Ord r) => Point 1 r `HasIntersectionWith` HalfOpenInterval r where
  (Point1 q) `intersects` i = q `stabsInterval` i

instance (Ord r) => ClosedInterval r `HasIntersectionWith` HalfOpenInterval r where
  closedInt `intersects` hoInt = case (closedInt^.start) `compareInterval` hoInt of
    LT -> case (closedInt^.end) `compareInterval` hoInt of
            LT -> False
            EQ -> True
            GT -> True
    EQ -> True
    GT -> False -- by invariant, closedInt^.end > closedInt.start, so they don't intersect
