{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Instances where

-- import Control.Lens
-- import D
-- import HGeometry.Vector.Class
import Test.QuickCheck
import HGeometry.Vector

--------------------------------------------------------------------------------

instance (Arbitrary r, Vector_ (Vector d r) d r) => Arbitrary (Vector d r) where
  arbitrary = generateA (const arbitrary)

-- instance ( forall r. VectorLike_ (Vector d r)
--          ) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = generateA (const gen)
--   -- I think this instance is unreachable, so let's comment it for now.
