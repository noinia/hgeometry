--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the polygon types in hgeometry
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Monotone.Instances
  (
  ) where

import Control.Monad.State
import HGeometry
import HGeometry.Polygon.Monotone
import System.Random.Stateful
import Test.QuickCheck hiding (vector)
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

instance (Uniform r, Ord r, Num r) => Arbitrary (MonotonePolygon (Point 2 r)) where
  arbitrary = do
                n <- max 3    <$> getSize
                g <- mkStdGen <$> arbitrary
                let (v, g') = runState randomNonZeroVector g
                pure $ evalState (randomMonotoneDirected n v) g'
