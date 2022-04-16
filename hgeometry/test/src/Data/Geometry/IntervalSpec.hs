{-# LANGUAGE ScopedTypeVariables #-}
module Geometry.IntervalSpec (spec) where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Geometry
import qualified Geometry.IntervalTree as IntTree
import           Geometry.SegmentTree (I(..))
import qualified Geometry.SegmentTree as SegTree
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Util

naive   :: (Ord r, Foldable f) => r -> f (Interval p r) -> [Interval p r]
naive q = filter (q `intersectsInterval`) . F.toList

sameAsNaive                 :: (Ord r, Ord p, Foldable f)
                            => f (Interval p r)
                            -> (r -> t -> [Interval p r], t)
                            -> r
                            -> Bool
sameAsNaive is (search,t) q = search q t `sameElems` naive q is


sameElems    :: Eq a => [a] -> [a] -> Bool
sameElems xs = null . difference xs


allSameAsNaive       :: (Ord r, Ord p)
                     => NonEmpty.NonEmpty (Interval p r) -> [r] -> Bool
allSameAsNaive is = all (sameAsNaive is (\q t -> _unI <$> SegTree.search q t
                                        , SegTree.fromIntervals' is))


allSameAsNaiveIT       :: (Ord r, Ord p)
                     => NonEmpty.NonEmpty (Interval p r) -> [r] -> Bool
allSameAsNaiveIT is = all (sameAsNaive is (\q t -> IntTree.search q t
                                         , IntTree.fromIntervals $ F.toList is))

spec :: Spec
spec = do
    describe "Same as Naive" $ do
      it "quickcheck segmentTree" $
        property $ \(Intervals is :: Intervals Word) -> allSameAsNaive is
      it "quickcheck IntervalTree" $
        property $ \(Intervals is :: Intervals Word) -> allSameAsNaiveIT is


newtype Intervals r = Intervals (NonEmpty.NonEmpty (Interval () r)) deriving (Show,Eq)

-- don't generate double open intervals, and don't generate intervals in which
-- one endpoint is open, the other is closed, but at the same point
instance (Arbitrary r, Ord r) => Arbitrary (Intervals r) where
  arbitrary = Intervals . NonEmpty.fromList <$> listOf1 (suchThat arbitrary p)
    where
      p (OpenInterval _ _) = False
      p (Interval s e)     = not (isOpen s /= isOpen e
                                  && s^.unEndPoint.core == e^.unEndPoint.core)
