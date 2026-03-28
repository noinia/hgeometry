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

import Control.Lens hiding (elements)
import Control.Monad.State
import Data.Aeson (eitherDecodeFileStrict)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (maybeToList)
import HGeometry
import HGeometry.Number.Real.Rational
import HGeometry.Number.Real.Interval
import HGeometry.Polygon.Class
import HGeometry.Polygon.Monotone
import HGeometry.Polygon.Simple
import Paths_hgeometry
import System.IO.Unsafe
import System.Random.Stateful
import Test.QuickCheck hiding (vector)
import Test.QuickCheck.Instances ()
import HGeometry.Polygon.Instances

--------------------------------------------------------------------------------

instance (Uniform r, Ord r, Num r) => Arbitrary (MonotonePolygon (Point 2 r)) where
  arbitrary = do
                n <- max 3    <$> getSize
                g <- mkStdGen <$> arbitrary
                let (v, g') = runState randomNonZeroVector g
                pure $ evalState (randomMonotoneDirected n v) g'
