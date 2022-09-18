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

  , HasStart(..)
  , HasEnd(..)
  , startAndEnd

  , inInterval

  , shiftLeft
  , flipInterval
  ) where

import Control.Lens
import Data.Tuple (swap)
import HGeometry.Boundary
import HGeometry.Interval.EndPoint
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | Things that have a start point
class HasStart seg p where
  -- | Lens to access the start point
  start :: Lens' seg p

-- | Things that have an end point
class HasEnd seg p where
  -- | Lens to access the ending point
  end :: Lens' seg p

-- | Get both the start and end of something that has a start and end.
startAndEnd   :: (HasStart seg p, HasEnd seg p) => seg -> (p,p)
startAndEnd i = (i^.start,i^.end)

--------------------------------------------------------------------------------

-- | A class for types representing Intervals
class ( HasStart (interval endPoint) endPoint
      , HasEnd   (interval endPoint) endPoint
      , EndPoint_ endPoint
      , NumType   (interval endPoint) ~ r
      ) => Interval_ interval endPoint where

  -- | Construct an interval given its start and end point.
  mkInterval :: endPoint -> endPoint -> interval endPoint

--------------------------------------------------------------------------------

-- | Pattern to match on intervals or construct them.
pattern Interval_     :: Interval_ interval endPoint
                      => endPoint -> endPoint -> interval endPoint
pattern Interval_ s t <- (startAndEnd -> (s,t))
  where
    Interval_ s t = mkInterval s t
{-# COMPLETE Interval_ #-}

-- | Compute where the given query value is with respect to the interval.
--
-- Note that even if the boundary of the interval is open we may
-- return "OnBoundary".
inInterval       :: forall interval endPoint r.
                    ( Ord r, Interval_ interval endPoint
                    , r ~ NumType (interval endPoint)
                    )
                 => r -> interval endPoint -> PointLocationResult
x `inInterval` i =
    case x `compare` (i^.start.endPoint @endPoint) of
      LT -> Outside
      EQ -> OnBoundary
      GT -> case x `compare` (i^.end.endPoint @endPoint) of
              LT -> Inside
              EQ -> OnBoundary
              GT -> Outside

-- | Shifts the interval to the left by delta
shiftLeft       :: (Num r, Functor interval, Functor endPoint)
                => r -> interval endPoint -> interval endPoint
shiftLeft delta = fmap (fmap subtract delta)

-- | Flips the start and endpoint of the interval.
flipInterval :: Interval_ interval endPoint => interval endPoint -> interval endPoint
flipInterval = uncurry mkInterval . swap . startAndEnd
