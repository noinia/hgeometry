{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry-point
--
--------------------------------------------------------------------------------
module HGeometry.Point.Instances where

import HGeometry.Point
import HGeometry.Vector.Instances ()
import Test.QuickCheck
--------------------------------------------------------------------------------

instance Arbitrary v => Arbitrary (PointF v) where
  arbitrary = Point <$> arbitrary
  shrink (Point v) = Point <$> shrink v
