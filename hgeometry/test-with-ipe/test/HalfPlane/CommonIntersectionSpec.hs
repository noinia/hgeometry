{-# LANGUAGE QuasiQuotes #-}
module HalfPlane.CommonIntersectionSpec(spec) where

import           Control.Lens
import           Control.Monad ((>=>))
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Vinyl
import           HGeometry.Boundary
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           Ipe
import           Ipe.Color
import           Paths_hgeometry
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "common halfplane intersection tests" $ do
         it "myHalfplanes test" $
           commonIntersection myHalfPlanes `shouldBe` theAnswer

myHalfPlanes :: [HalfPlane R]
myHalfPlanes = [ Below $ LineEQ 1    1
               , Below $ LineEq (-1) 2
               ]

theAnswer :: CommonIntersection (HalfPlane R) R
theAnswer = Unbounded $ LowerChain (Seq.fromList $
                                    [ (myHalfPlanes !! 0, Point 1 1)
                                    ]
                                    (myHalfPlanes !! 1)
