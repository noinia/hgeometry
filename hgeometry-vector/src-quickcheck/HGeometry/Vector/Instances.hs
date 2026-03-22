{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry-vector
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Instances where

import Control.Lens
import HGeometry.Vector
import Test.QuickCheck

--------------------------------------------------------------------------------

instance (Arbitrary r, Vector_ (Vector d r) d r) => Arbitrary (Vector d r) where
  arbitrary = generateA (const arbitrary)
  shrink v = [ v&component' i .~ x'
             | (i,x) <- v^..components.withIndex
             , x' <- shrink x
             ]

-- instance ( forall r. VectorLike_ (Vector d r)
--          ) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = generateA (const gen)
--   -- I think this instance is unreachable, so let's comment it for now.
