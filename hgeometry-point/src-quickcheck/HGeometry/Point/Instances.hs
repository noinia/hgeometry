{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Point.Instances where

import GHC.TypeLits
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.Instances ()
import Test.QuickCheck
--------------------------------------------------------------------------------

instance Arbitrary v => Arbitrary (PointF v) where
  arbitrary = Point <$> arbitrary
