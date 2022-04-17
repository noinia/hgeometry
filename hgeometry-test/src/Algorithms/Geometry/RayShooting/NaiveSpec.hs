module Algorithms.Geometry.RayShooting.NaiveSpec where

import Algorithms.Geometry.RayShooting.Naive
import Control.Lens
import Data.Bifunctor
import Data.Ext
import Geometry.HalfLine
import Geometry.Line
import Geometry.LineSegment
import Geometry.Point
import Geometry.Polygon
import Geometry.PolygonSpec() -- import the arbitrary instance
import           Geometry.Vector
import           Data.Intersection
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.RealNumber.Rational
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Ipe
import           Test.Hspec
import           Test.QuickCheck
import           Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

testPath = "src/Algorithms/Geometry/RayShooting/"

spec :: Spec
spec = describe "Naive Rayshooting tests" $ do
         manualSpec (testPath <> "manual.ipe")
         it "from inside polygon we always hit something" $
           property $ \(pg :: SimplePolygon () R) v ->
                        let ray = HalfLine (pickPoint pg) (v :: Vector 2 R)
                        in quadrance v > 0 ==> isJust (firstHit' ray pg)

data TestCase r = TestCase [LineSegment 2 () r :+ IpeAttributes Path r]
                           (HalfLine 2 r :+ IpeAttributes Path r)
                deriving (Show,Eq)

isRay (_ :+ ats) = has (_Attr SArrow) ats

toRay s = let (Line p v) = supportingLine s in HalfLine p v


manualSpec    :: FilePath -> Spec
manualSpec fp = do allSegs <- runIO $ readAllFrom fp
                   let (rays,segs) = List.partition isRay allSegs
                       testCases   = map (TestCase segs . over core toRay) rays
                   mapM_ ipeTestcase testCases




ipeTestcase :: TestCase R -> Spec
ipeTestcase (TestCase segs (ray :+ ats)) =
    it ("Naive right ray shooting " <> show maybeColor) $
      (firstHitSegments ray segs >>= (^?extra._Attr SStroke))
      `shouldBe`
      maybeColor
  where
    maybeColor = ats^?_Attr SStroke
