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
  ( Interval_, pattern Interval_
  , IntervalLike_(..)

  , ClosedInterval_(..), pattern ClosedInterval_
  , clampTo

  , OpenInterval_(..), pattern OpenInterval_

  , HasStart(..)
  , HasEnd(..)
  , startAndEnd

  , EndPointOf
  , HasStartPoint(..)
  , HasEndPoint(..)
  , startAndEndPoint


  , inInterval
  , compareInterval

  , shiftLeft, shiftRight
  -- , flipInterval
  , duration
  , module HGeometry.Interval.EndPoint
  ) where

import Control.Lens
import Data.Kind (Type,Constraint)
import HGeometry.Boundary
import HGeometry.Ext
import HGeometry.Interval.EndPoint
import HGeometry.Properties

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Interval.EndPoint
-- >>> import HGeometry.Interval

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

-- | Things that have a start point
class HasStartPoint seg p | seg -> p where
  -- | Lens to access the start point
  startPoint :: Lens' seg p

-- | Things that have an end point
class HasEndPoint seg p | seg -> p where
  -- | Lens to access the ending point
  endPoint :: Lens' seg p

-- | Get both the start and end of something that has a start and end.
startAndEndPoint   :: (HasStartPoint seg p, HasEndPoint seg p) => seg -> (p,p)
startAndEndPoint i = (i^.startPoint, i^.endPoint)

-- | type family to declare the type of endpoint for an interval, the
-- idea is to define this as one of the endpoinst form the Endpoints module
type family EndPointOf interval

--------------------------------------------------------------------------------

-- | A class for types representing interval like objects
type IntervalLike_ :: Type -> Type -> Constraint
class ( HasStart interval point, HasStartPoint interval (EndPointOf interval)
      , HasEnd   interval point, HasEndPoint   interval (EndPointOf interval)
      , EndPoint_ (EndPointOf interval), IxValue (EndPointOf interval) ~ point
      ) => IntervalLike_ interval point | interval -> point where

  -- | Construct an interval given its start and end point.
  --
  -- pre: start < end
  mkInterval :: EndPointOf interval -> EndPointOf interval -> interval

--------------------------------------------------------------------------------

-- | A class for types representing Intervals
type Interval_ :: Type -> Type -> Constraint
class ( IntervalLike_ interval r
      , NumType interval ~ r
      ) => Interval_ interval r | interval -> r where


--------------------------------------------------------------------------------
-- * Closed Intervals

-- | A class representing closed intervals, i.e. intervals that include their endpoints
type ClosedInterval_ :: Type -> Type -> Constraint
class (Interval_ interval r
      , EndPointOf interval ~ EndPoint Closed r
      ) => ClosedInterval_ interval r where
  -- | Construct an interval given its start and end point.
  mkClosedInterval     :: r -> r -> interval
  mkClosedInterval s e = mkInterval (ClosedE s) (ClosedE e)
  {-# MINIMAL #-}

-- | Pattern matching on an arbitrary closed interval
pattern ClosedInterval_     :: ClosedInterval_ interval r => r -> r -> interval
pattern ClosedInterval_ l u <- (startAndEnd -> (l,u))
  where
    ClosedInterval_ l u = mkClosedInterval l u
{-# COMPLETE ClosedInterval_ #-}
{-# INLINE ClosedInterval_ #-}

-- | Clamps a value to an interval. I.e. if the value lies outside the range we
-- report the closest value "in the range".
--
-- >>> clampTo (ClosedInterval 0 10) 20
-- 10
-- >>> clampTo (ClosedInterval 0 10) (-20)
-- 0
-- >>> clampTo (ClosedInterval 0 10) 5
-- 5
clampTo                         :: (ClosedInterval_ interval r, Ord r) => interval -> r -> r
clampTo (ClosedInterval_ l u) x = (x `max` l) `min` u

--------------------------------------------------------------------------------


-- | A class representing open intervals, i.e. intervals that exclude their endpoints
class ( Interval_ interval r
      , EndPointOf interval ~ EndPoint Open r
      ) => OpenInterval_ interval r | interval -> r where
  -- | Construct an interval given its start s and end point t.
  --
  -- pre: s < t
  mkOpenInterval     :: r -> r -> interval
  mkOpenInterval s e = mkInterval (OpenE s) (OpenE e)
  {-# MINIMAL #-}

-- | Pattern matching on an arbitrary open interval
pattern OpenInterval_     :: OpenInterval_ interval r => r -> r -> interval
pattern OpenInterval_ l u <- (startAndEnd -> (l,u))
  where
    OpenInterval_ l u = mkOpenInterval l u
{-# COMPLETE OpenInterval_ #-}

--------------------------------------------------------------------------------

-- | Pattern to match on intervals or construct them.
pattern Interval_     :: Interval_ interval r
                      => EndPointOf interval -> EndPointOf interval -> interval
pattern Interval_ s t <- (startAndEndPoint -> (s,t))
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


-- | test if te point appears before (=LT), in (=EQ), or after (=GT) te interval.
--
-- >>> 1 `compareInterval` (OpenInterval 0 2)
-- EQ
-- >>> 1 `compareInterval` (OpenInterval 0 1)
-- GT
-- >>> 1 `compareInterval` (ClosedInterval 0 1)
-- EQ
-- >>> 10 `compareInterval` (OpenInterval 1 10)
-- GT
-- >>> 10 `compareInterval` (ClosedInterval 0 1)
-- GT
compareInterval     :: (Ord r, Interval_ interval r) => r -> interval -> Ordering
compareInterval q i = case q `compare` (i^.start) of
      LT -> LT
      EQ -> if i^.startPoint.to endPointType == Open then LT else EQ
            -- since the interval is non-degenerate, the right endpoint must be strictly
            -- larger than the left endpoint
      GT -> case q `compare` (i^.end) of
              LT -> EQ
              EQ -> if i^.endPoint.to endPointType == Open then GT else EQ
              GT -> GT

-- | Shifts the interval to the left by delta
shiftLeft         :: ( Num r, Interval_ interval r) => r -> interval -> interval
shiftLeft delta i = i&start %~ subtract delta
                     &end   %~ subtract delta

-- | Shifts the interval to the right by delta
shiftRight         :: ( Num r, Interval_ interval r ) => r -> interval -> interval
shiftRight delta i = i&start %~ (+ delta)
                      &end   %~ (+ delta)

-- -- | Flips the start and endpoint of the interval.
-- flipInterval :: Interval_ interval r => interval -> interval
-- flipInterval = uncurry mkInterval . swap . startAndEndPoint

-- | Get the duration, or length of an interval.
duration   :: (Interval_ interval r, Num r) => interval -> r
duration i = i^.end - i^.start


--------------------------------------------------------------------------------

instance HasStart seg p => HasStart (seg :+ extra) p where
  start = core.start
instance HasEnd seg p => HasEnd (seg :+ extra) p where
  end = core.end
instance HasStartPoint seg p => HasStartPoint (seg :+ extra) p where
  startPoint = core.startPoint
instance HasEndPoint seg p => HasEndPoint (seg :+ extra) p where
  endPoint = core.endPoint

type instance EndPointOf (interval :+ extra) = EndPointOf interval

instance ( IntervalLike_ interval point
         , Monoid extra
         ) => IntervalLike_ (interval :+ extra) point where
  mkInterval s t = mkInterval s t :+ mempty

instance ( Interval_ interval r
         , Monoid extra
         ) => Interval_ (interval :+ extra) r

instance ( ClosedInterval_ interval r
         , Monoid extra
         ) => ClosedInterval_ (interval :+ extra) r where
  mkClosedInterval s t = mkClosedInterval s t :+ mempty

instance ( OpenInterval_ interval r
         , Monoid extra
         ) => OpenInterval_ (interval :+ extra) r where
  mkOpenInterval s t = mkOpenInterval s t :+ mempty
