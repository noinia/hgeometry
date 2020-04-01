module Data.Geometry.BezierSplineSpec where


import           Control.Lens
import qualified Data.Geometry.BezierMaarten as Maarten
import           Data.Geometry.BezierSpline
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.RealNumber.Rational
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5

toMaartenBezier = Bezier . ctrlPts

ctrlPts :: BezierSpline n d r -> [Point d r]
ctrlPts = b^..controlPoints

newtype T = T R deriving (Show,Eq,Ord,Num,Real,Fractional)

instance Arbitrary T where
  arbitrary = realToFrac <$> choose (0,1 :: Double)

spec :: Spec
spec = describe "BezierSpline" $ do
         property "evaluate" $ \(T t) (b :: BezierSpline 3 2 R) ->
                                 evaluate t b `shouldBe` Maarten.evaluate t (toMaartenBezier b)
