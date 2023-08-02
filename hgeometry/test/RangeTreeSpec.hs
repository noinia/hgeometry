module RangeTreeSpec where

import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.Measured
import           HGeometry.Measured.Report
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.RangeTree
import           Test.Hspec
import           Test.Hspec.QuickCheck

--------------------------------------------------------------------------------

type R = Int

spec :: Spec
spec = describe "RangeTree tests" $ do
         it "manual test" $ do
           query (ClosedInterval 2 20) myTree `souldBe` (Count 3)
         prop "same as naive" $
           \(qs :: Interval AnEndPoint R) (pts :: NonEmpty (Point 2 R)) ->
             let t = buildRangeTree pts
             in all (\q -> let Report res = query q t
                           in Set.fromList res == Set.fromList (naive q pts)
                    ) qs

naive                     :: Interval AnEndPoint R -> f (Point 2 R) -> [Point 2 R]
naive q (F.toList -> pts) = filter (\p -> projectPoint p `intersects` q) pts


myTree :: (Measured f (Point 2 R), Monoid f (Point 2 R)) => RangeTree f (Point 2 R)
myTree = buildRangeTree myPoints

myPoints :: NonEmpty (Point 2 Int)
myPoints = NonEmpty.fromList
           [ origin
           , Point2 10 20
           , Point2 100 1
           , Point2 4 2
           , Point2 15 10
           ]
