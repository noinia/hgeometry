{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Instances where

import Control.Lens
import D
import HGeometry.Vector.Class
import R
import Test.QuickCheck
import Vector

--------------------------------------------------------------------------------

instance ( Arbitrary R
         , VectorLike_ (Vector D R)
         , IxValue (Vector D R) ~ R
         ) => Arbitrary (Vector D R) where
  arbitrary = generateA (const arbitrary)

-- instance ( forall r. VectorLike_ (Vector d r)
--          ) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = generateA (const gen)
--   -- I think this instance is unreachable, so let's comment it for now.
