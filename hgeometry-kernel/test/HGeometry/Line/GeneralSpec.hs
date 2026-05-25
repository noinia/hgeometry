module HGeometry.Line.GeneralSpec
  (spec
  ) where

import HGeometry.HyperPlane.Class
import HGeometry.Kernel.Instances ()
import HGeometry.Line.General
import HGeometry.Number.Real.Rational (RealNumber)
import HGeometry.Vector
import Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = do
  describe "Line General Form" $ do
    it "should be vertical" $ do
      let l = VerticalLineThrough 5
      asGeneralLine l `shouldBe` l
    it "hyperPlaneEq vertical line" $
      hyperPlaneEquation (VerticalLineThrough 5) `shouldBe` (Vector3 1 0 (-5))


asGeneralLine :: HyperPlane_ hyperPlane 2 R => hyperPlane -> VerticalOrLineEQ R
asGeneralLine = hyperPlaneFromEquation . hyperPlaneEquation
