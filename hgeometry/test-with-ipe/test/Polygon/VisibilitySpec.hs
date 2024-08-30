{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.VisibilitySpec where

import Data.Maybe
import Golden
import HGeometry.Ball
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Number.Rational.RealNumber
import HGeometry.Point
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "visibility graph / visibility polygon" $ do
         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeContentGolden { name = [osp|visibility|] })
             (concat
             [
             ])

-- out = pg
