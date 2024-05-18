{-# LANGUAGE QuasiQuotes #-}
module HalfPlane.CommonIntersectionSpec(spec) where

import           Control.Lens hiding (below)
import           Control.Monad ((>=>))
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Vinyl
import           HGeometry.Boundary
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HalfPlane.CommonIntersection
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LineEQ
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Sequence.Alternating
import           Ipe
import           Ipe.Color
import           Paths_hgeometry
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()
--------------------------------------------------------------------------------

type R = RealNumber 5

type HalfPlane r = HalfSpaceF (VerticalOrLineEQ r)

spec :: Spec
spec = describe "common halfplane intersection tests" $ do
         it "myHalfplanes test" $
           commonIntersection myHalfPlanes `shouldBe` theAnswer

myHalfPlanes :: NonEmpty (HalfPlane R)
myHalfPlanes = NonEmpty.fromList
               [ below $ LineEQ 1    1
               , below $ LineEQ (-1) 2
               ]

theAnswer :: CommonIntersection (HalfPlane R) R
theAnswer = Unbounded . Chain
          $ Alternating (myHalfPlanes NonEmpty.!! 0)
                        (Seq.fromList $ [ (Point2 1 1, myHalfPlanes NonEmpty.!! 1)
                                        ]
                        )


--------------------------------------------------------------------------------
-- * some helpers for producing Halfplanes

below :: LineEQ r -> HalfPlane r
below = HalfSpace Negative . NonVertical

above :: LineEQ r -> HalfPlane r
above = HalfSpace Positive . NonVertical

leftOf :: r -> HalfPlane r
leftOf = HalfSpace Negative . VerticalLineThrough

rightOf :: r -> HalfPlane r
rightOf = HalfSpace Positive . VerticalLineThrough



--------------------------------------------------------------------------------

-- FIXME: this instance does not really make sene I think, but whatever
instance Num r => Default (HalfPlane r) where
  def = HalfSpace Negative (VerticalLineThrough 0)
instance Num r => Default (LineEQ r) where
  def = LineEQ 1 0
