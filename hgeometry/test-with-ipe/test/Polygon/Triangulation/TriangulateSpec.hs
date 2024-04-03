{-# LANGUAGE OverloadedStrings #-}
module Polygon.Triangulation.TriangulateSpec (spec) where

import Control.Lens
import HGeometry
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
-- import HGeometry.Polygon.Instances ()
import HGeometry.Polygon.Triangulation
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = do
  pure ()
