{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Instances where

import HGeometry.Vector.Class
import Test.QuickCheck

--------------------------------------------------------------------------------

instance ( Arbitrary r
         , VectorLike_ (Vector d r)
         ) => Arbitrary (Vector d r) where
  arbitrary = generateA (const arbitrary)

-- instance ( forall r. VectorLike_ (Vector d r)
--          ) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = generateA (const gen)
--   -- I think this instance is unreachable, so let's comment it for now.
