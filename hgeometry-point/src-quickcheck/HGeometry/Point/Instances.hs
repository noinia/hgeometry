{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Point.Instances where

import HGeometry.Point
import HGeometry.Vector.Instances ()
import Test.QuickCheck
--------------------------------------------------------------------------------

instance Arbitrary v => Arbitrary (PointF v) where
  arbitrary = Point <$> arbitrary
