module RangeTreeSpec where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           HGeometry.Instances ()
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
           query (ClosedInterval 2 20) myTree `shouldBe` (Count 3)
         prop "same as naive" $
           \(qs :: NonEmpty (ClosedInterval R)) (pts :: NonEmpty (Point 2 R)) ->
             let t = buildRangeTree pts
             in all (\q -> let Report res = query q t
                           in Set.fromList res == Set.fromList (naive q pts)
                    ) qs

         it "bugfix" $
           let Report res = query bugQuery bugTree
           in Set.fromList res `shouldBe` Set.fromList (naive bugQuery bugPts)
         it "bugfix2" $
           let Report res = query bugQuery2 bugTree2
           in Set.fromList res `shouldBe` Set.fromList (naive bugQuery2 bugPts2)
         runIO $ do
           print bugTree2

bugQuery :: ClosedInterval Int
bugQuery = Interval (ClosedE 3) (ClosedE 4)

bugTree = buildRangeTree bugPts
bugPts :: NonEmpty (Point 2 Int)
bugPts = Point2 3 0 :| [Point2 2 4]

bugTree2 = buildRangeTree bugPts2
bugQuery2 :: ClosedInterval Int
bugQuery2 = Interval (ClosedE 3) (ClosedE 7)
bugPts2 :: NonEmpty (Point 2 Int)
bugPts2 = Point2 7 (-2) :| [Point2 (-5) (-6),Point2 1 (-2),Point2 8 3,Point2 5 6]




naive                     :: (Foldable f, Interval_ interval r, Ord r)
                          => interval -> f (Point 2 r) -> [Point 2 r]
naive q (F.toList -> pts) = filter (\p -> (p^.xCoord) `stabsInterval` q) pts


myTree :: (Measured f (Point 2 R), Semigroup (f (Point 2 R))) => RangeTree f (Point 2 R)
myTree = buildRangeTree myPoints

myPoints :: NonEmpty (Point 2 Int)
myPoints = NonEmpty.fromList
           [ origin
           , Point2 10 20
           , Point2 100 1
           , Point2 4 2
           , Point2 15 10
           ]
