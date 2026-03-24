--------------------------------------------------------------------------------
-- |
-- Module      :  IntervalReal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines the Real-number type
--
--------------------------------------------------------------------------------
module IntervalReal where

import HGeometry.Number.Real.Rational
import HGeometry.Number.Real.Interval

-- | The Real number type
type R = IntervalReal (RealNumber 5)
