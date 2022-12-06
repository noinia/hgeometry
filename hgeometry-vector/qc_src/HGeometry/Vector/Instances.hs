{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Instances where

import GHC.TypeLits
import HGeometry.Vector
import Test.QuickCheck

--------------------------------------------------------------------------------

instance (Arbitrary r, KnownNat d, OptVector_ d r) => Arbitrary (Vector d r) where
  arbitrary = generateA (const arbitrary)

-- instance (KnownNat d) => Arbitrary1 (Vector d) where
--   liftArbitrary g =
