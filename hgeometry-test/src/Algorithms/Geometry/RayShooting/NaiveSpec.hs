module Algorithms.Geometry.RayShooting.NaiveSpec where

import           Algorithms.Geometry.RayShooting.Naive
import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.HalfLine
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Intersection
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Ipe
import           Test.HSpec

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "Naive Rayshooting tests" $ do
         manualSpec "manual.ipe"
         it "from inside polygon we always hit something" $
           property $ \pg v ->  let ray = HalfLine (pickPoint pg) (v :: Vector 2 R)
                                in isJust (firstHit' ray pg)

data TestCase r = TestCase [LineSegment 2 () r :+ IpeAttributes Path r]
                           (HalfLine 2 r :+ IpeAttributes Path r)
                deriving (Show,Eq)

isRay (_ :+ ats) = has (_Attr SForwardArrow) ats


manualSpec    :: FilePath -> Spec
manualSpec fp = do allSegs <- readAllFrom fp
                   let (rays,segs) = List.partition isRay allSegs
                       testCases   = map (TestCase segs) rays
                   mapM_ ipeTestcase testCases


ipeTestcase :: TestCase R -> Spec
ipeTestcase (TestCase segs (ray :+ ats)) =
    it ("Naive right ray shooting " <> show maybeColor) $
      firstHitSegments ray segs ^?extra
      `shouldBe`
      maybeColor
  where
    maybeColor = ats^?_Attr SSrroke
