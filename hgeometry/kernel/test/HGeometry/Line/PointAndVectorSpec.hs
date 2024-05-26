module HGeometry.Line.PointAndVectorSpec(spec) where


-- import Control.Lens
import HGeometry.Kernel.Instances ()
import HGeometry.Line.PointAndVector
import HGeometry.Number.Real.Rational (RealNumber)
import HGeometry.Point
-- import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = do
  describe "Line Point and Vector" $ do
    prop "perpendicularTo origin correct" $
      \(l@(LinePV p _) :: LinePV 2 R) ->
        let LinePV q _ = perpendicularTo l
        in p === q
    prop "perpendicularTo direction correct (i..e v points in to left halfspace)" $
      \(l@(LinePV p v) :: LinePV 2 R) ->
        let m = perpendicularTo l
        in p .+^ v `onSide` m === LeftSide
