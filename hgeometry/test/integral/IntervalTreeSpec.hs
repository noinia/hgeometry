module IntervalTreeSpec(spec) where

import qualified Data.Set as Set
import           HGeometry.Instances ()
import           HGeometry.Interval
import           HGeometry.IntervalTree
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()
import           R
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "intervaltree tests" $ do
         prop "same as naive" $ \q qs' (ints :: [ClosedInterval Int]) -> let qs = q:qs' in
           queryAll qs (fromIntervals ints) `shouldBe` naive qs ints

naive       :: [R] -> [ClosedInterval Int] -> Set.Set (ClosedInterval Int)
naive qs is = Set.fromList [ i | q <- qs, i <- is, q `stabsInterval` i ]

queryAll      :: (Ord r, Ord interval, ClosedInterval_ interval r)
              => [r] -> IntervalTree interval r -> Set.Set interval
queryAll qs t = foldMap (\q -> Set.fromList $ stab q t) qs
