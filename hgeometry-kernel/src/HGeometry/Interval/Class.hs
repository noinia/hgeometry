--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval.Class
  ( Interval_(..), pattern Interval_
  , ClosedInterval_(..)
  , OpenInterval_(..)
  , IntervalFor

  , HasStart(..)
  , HasEnd(..)
  , startAndEnd

  , inInterval

  , shiftLeft
  , flipInterval
  , duration
  , module HGeometry.Interval.EndPoint
  ) where


import Control.Lens
import Data.Tuple (swap)
import Data.Kind (Type,Constraint)
import HGeometry.Boundary
import HGeometry.Interval.EndPoint
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | Things that have a start point
class HasStart seg p | seg -> p where
  -- | Lens to access the start point
  start :: Lens' seg p

-- | Things that have an end point
class HasEnd seg p | seg -> p where
  -- | Lens to access the ending point
  end :: Lens' seg p

-- | Get both the start and end of something that has a start and end.
startAndEnd   :: (HasStart seg p, HasEnd seg p) => seg -> (p,p)
startAndEnd i = (i^.start,i^.end)

--------------------------------------------------------------------------------

-- type family IntervalOf (et :: EndPointType) r

-- | Types that output intervals should use this type
type family IntervalFor g

-- | A class for types representing Intervals
type Interval_ :: Type -> Type -> Constraint
class ( HasStart interval endPoint
      , HasEnd   interval endPoint
      , EndPoint_ endPoint
      , NumType interval ~ NumType endPoint
      ) => Interval_ interval endPoint | interval -> endPoint where

  -- | Construct an interval given its start and end point.
  mkInterval :: endPoint -> endPoint -> interval


type ClosedInterval_ :: Type -> Type -> Constraint
class Interval_ interval (EndPoint Closed r) =>
      ClosedInterval_ interval r | interval -> r where
  -- | Construct an interval given its start and end point.
  mkClosedInterval     :: r -> r -> interval
  mkClosedInterval s e = mkInterval (ClosedE s) (ClosedE e)
  {-# MINIMAL #-}

class Interval_ interval (EndPoint Open r) =>
      OpenInterval_ interval r | interval -> r where
  -- | Construct an interval given its start and end point.
  mkOpenInterval     :: r -> r -> interval
  mkOpenInterval s e = mkInterval (OpenE s) (OpenE e)
  {-# MINIMAL #-}

--------------------------------------------------------------------------------

-- | Pattern to match on intervals or construct them.
pattern Interval_     :: Interval_ interval endPoint
                      => endPoint -> endPoint -> interval
pattern Interval_ s t <- (startAndEnd -> (s,t))
  where
    Interval_ s t = mkInterval s t
{-# COMPLETE Interval_ #-}


-- | Compute where the given query value is with respect to the interval.
--
-- Note that even if the boundary of the interval is open we may
-- return "OnBoundary".
inInterval       :: forall interval r.
                    ( Ord r
                    , Interval_ interval r
                    )
                 => r -> interval -> PointLocationResult
x `inInterval` i =
    case x `compare` (i^.start) of
      LT -> Outside
      EQ -> OnBoundary
      GT -> case x `compare` (i^.end) of
              LT -> Inside
              EQ -> OnBoundary
              GT -> Outside


-- | Shifts the interval to the left by delta
shiftLeft       :: ( Num r
                   , Functor interval
                   )
                => r -> interval r -> interval r
shiftLeft delta = fmap (subtract delta)

-- | Flips the start and endpoint of the interval.
flipInterval :: Interval_ interval endPoint => interval -> interval
flipInterval = uncurry mkInterval . swap . startAndEnd

-- | Get the duration, or length of an interval.
duration   :: forall interval endPoint. ( Interval_ interval endPoint
                                        , Num (NumType interval)
              ) => interval -> NumType interval
duration i = i^.end.(endPoint @_ @endPoint) - i^.start.(endPoint @_ @endPoint)
