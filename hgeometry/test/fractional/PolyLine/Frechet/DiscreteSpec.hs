module PolyLine.Frechet.DiscreteSpec where

import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.PolyLine
import           HGeometry.PolyLine.Frechet.Discrete
import           HGeometry.Point
import           Test.Hspec

--------------------------------------------------------------------------------

type R = Int

spec :: Spec
spec = do
         it "trivial" $ do
           let ta = polyLineFromPoints' $ [origin, Point2 5 5]
               tb = polyLineFromPoints'  $ [Point2 0 1, Point2 5 6, Point2 6 6]
           discreteFrechetDistance ta tb `shouldBe` 2
         it "trivial 2" $ do
           let ta = polyLineFromPoints' [Point2 x 0     | x <- [1..10]]
               tb = polyLineFromPoints' [Point2 x (y x) | x <- [1..10]]
               y x = if x == 5 then 5 else 1
           discreteFrechetDistance ta tb `shouldBe` 25

discreteFrechetDistance :: PolyLine (Point 2 R) -> PolyLine (Point 2 R) -> R
discreteFrechetDistance = frechetDistanceWith squaredEuclideanDist

polyLineFromPoints' :: [Point 2 R] -> PolyLine (Point 2 R)
polyLineFromPoints' = polyLineFromPoints . NonEmpty.fromList
