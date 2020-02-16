module Algorithms.Geometry.ConvexHull.ConvexHull3DSpec where

import qualified Algorithms.Geometry.ConvexHull.KineticDivideAndConquer as DivAndConc
import qualified Algorithms.Geometry.ConvexHull.Naive as Naive
import           Algorithms.Geometry.ConvexHull.Naive (ConvexHull)
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import qualified Data.Set as Set
import           Data.Util
-- import           Algorithms.Util
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "3D ConvexHull tests" $ do
         it "manual on myPts"  $ (H $ Naive.lowerHull' myPts)  `shouldBe` myHull
         it "manual on myPts'" $ (H $ Naive.lowerHull' myPts') `shouldBe` myHull'

         it "same as naive on myPts " $ sameAsNaive myPts
         it "same as naive on myPts'" $ sameAsNaive myPts'
         it "same as naive quickcheck" $ property $ \(HI pts) -> sameAsNaive pts

newtype HullInput = HI (NonEmpty (Point 3 (RealNumber 10) :+ Int)) deriving (Eq,Show)

instance Arbitrary HullInput where
  arbitrary = (\as bs -> fromPts $ as <> bs) <$> setOf 3 arbitrary <*> arbitrary
    where
      fromPts pts = HI . NonEmpty.fromList
                 $ zipWith (:+) (fmap (realToFrac @Int @(RealNumber 10)) <$> Set.toList pts) ([1..])


sameAsNaive pts = (H $ DivAndConc.lowerHull' pts) `shouldBe` (H $ Naive.lowerHull' pts)

newtype Hull p r = H (ConvexHull 3 p r) deriving (Show)

instance (Eq r, Ord p) => Eq (Hull p r) where
  (H ha) == (H hb) = f ha == f hb
    where
      f = List.sortOn g . map reorder
      g = fmap (^.extra) . (^._TriangleThreePoints)

reorder                  :: Ord p => Triangle 3 p r -> Triangle 3 p r
reorder (Triangle p q r) = let [p',q',r'] = List.sortOn (^.extra) [p,q,r] in Triangle p' q' r'


myPts :: NonEmpty (Point 3 Double :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]

toTri       :: Eq a =>  NonEmpty (Point d r :+ a) -> Three a -> Triangle d a r
toTri pts t = let pt i = head $ NonEmpty.filter (\t -> t^.extra == i) pts
              in (t&traverse %~ pt)^.from _TriangleThreePoints

myHull :: Hull Int Double
myHull = H . map (toTri myPts) $ [ Three 1 2 3
                                 , Three 2 3 4
                                 , Three 0 1 2
                                 , Three 0 2 4
                                 ]

myPts' :: NonEmpty (Point 3 Double :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 0
                             , Point3 12 1  1  :+ 3
                             ]

myHull' :: Hull Int Double
myHull' = H . map (toTri myPts') $ [ Three 1 2 3
                                   , Three 0 1 2
                                   , Three 0 2 3
                                   ]


--------------------------------------------------------------------------------

-- | Generates a set of n elements (all being different), using the
-- given generator.
setOf    :: Ord a => Int -> Gen a -> Gen (Set.Set a)
setOf n g = buildSet mempty <$> do sz <- getSize
                                   infiniteListOf (resize (max sz n) g)
  where
    buildSet s (x:xs) | length s == n = s
                      | otherwise     = let s' = Set.insert x s in buildSet s' xs
    buildSet _  _                     = error "setOf: absurd"
