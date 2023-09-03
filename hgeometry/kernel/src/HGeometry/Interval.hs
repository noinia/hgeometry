--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval
  ( Interval(Interval,ClosedInterval,OpenInterval)
  , ClosedInterval, OpenInterval
  , HalfOpenInterval(HalfOpenInterval)
  , module HGeometry.Interval.Class
  , IntersectionOf(..)
  ) where

import HGeometry.Intersection
import HGeometry.Interval.Class
import HGeometry.Interval.EndPoint ()
import HGeometry.Interval.Internal
import HGeometry.Interval.HalfOpen
