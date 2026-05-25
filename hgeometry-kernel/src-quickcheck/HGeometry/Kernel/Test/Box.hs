--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Kernel.Test.Box
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper Utilities
--
--------------------------------------------------------------------------------
module HGeometry.Kernel.Test.Box where

import Control.Lens
import Data.Ratio
import HGeometry.Box
import HGeometry.Point
import HGeometry.Vector
import Test.QuickCheck

--------------------------------------------------------------------------------

--  | Generate an arbitrary point in the given rectangle.
arbitraryPointInBoundingBox   :: Rectangle (Point 2 Rational) -> Gen (Point 2 Rational)
arbitraryPointInBoundingBox b = do
  ZeroToOne rX <- arbitrary
  ZeroToOne rY <- arbitrary
  let minPt        = b^.minPoint
      offsetVector = Vector2 (width b * rX) (height b * rY)
  pure $ minPt .+^ offsetVector


newtype ZeroToOne = ZeroToOne Rational

instance Show ZeroToOne where
  show (ZeroToOne r) = show r

instance Arbitrary ZeroToOne where
  arbitrary = do
    k <- chooseInteger (0, granularity)
    pure $ ZeroToOne $ k % granularity
    where
      granularity = 1000000
  shrink (ZeroToOne 1) = []
  shrink (ZeroToOne 0) = []
  shrink (ZeroToOne r) = [ ZeroToOne $ div (numerator r) 2 % div (denominator r) 2]
