--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry
--
--------------------------------------------------------------------------------
module HGeometry.Instances
  (
  ) where

import HGeometry.Kernel.Instances ()
import HGeometry.Graphics.Camera
import Test.QuickCheck
import HGeometry.Vector

instance (Arbitrary r, Ord r, Num r) => Arbitrary (Camera r) where
  arbitrary = do p <- arbitrary
                 n <- arbitrary
                 u <- arbitrary
                 Positive fd <- arbitrary
                 Positive near <- arbitrary
                 far           <- arbitrary `suchThat` (> near)
                 vp            <- arbitrary `suchThat` (> zero)
                 pure $ Camera p n u fd near far vp
