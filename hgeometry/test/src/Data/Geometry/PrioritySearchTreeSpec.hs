module Data.Geometry.PrioritySearchTreeSpec where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.PrioritySearchTree
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (Down(..), comparing)
import           Data.Range
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances()

--------------------------------------------------------------------------------

newtype Unique a = Unique (NonEmpty a) deriving (Show,Eq,Ord)

instance (Arbitrary a, Ord a) => Arbitrary (Unique a) where
  arbitrary = do s  <- arbitrary
                 s' <- if Set.null s then Set.singleton <$> arbitrary else pure s
                 Unique . NonEmpty.fromList <$> shuffle (Set.toList s')


newtype Pts r = Pts (NonEmpty (Point 2 r :+ ())) deriving (Show,Eq,Ord)

instance (Arbitrary r, Ord r) => Arbitrary (Pts r) where
  arbitrary = f <$> arbitrary <*> arbitrary
    where
      -- make sure all x-coordiantes are unique
      f                :: Unique r -> NonEmpty r -> Pts r
      f (Unique xs) ys = Pts $ NonEmpty.zipWith (\x y -> ext $ Point2 x y) xs ys

--------------------------------------------------------------------------------


spec :: Spec
spec = do
  describe "Priority Search Tree" $ do
    it "reporting tests; same as naive" $ do
      let t  = createTree pts
          qs = [ (ClosedRange 1 10, 3)
               , (ClosedRange 1 11, 3)
               ]
      forM_ qs $ \q ->
        queryRange q t `shouldBe` naive q pts
    it "quickcheck: same elems as naive" $
      property $ \(Pts qPts :: Pts Int) q -> do
        Set.fromList (queryRange q $ createTree qPts)
          `shouldBe`
          Set.fromList (naive q qPts)
    it "quickcheck: decreasing-y (same as naive)" $
      property $ \(Pts qPts :: Pts Int) q -> do
        map (^.core.yCoord) (queryRange q (createTree qPts))
          `shouldBe`
          map (^.core.yCoord) (naive q qPts)

pts :: NonEmpty (Point 2 Int :+ ())
pts = NonEmpty.fromList . map ext $ [ origin
                                    , Point2 (-1) 5
                                    , Point2 2 10
                                    , Point2 3 0
                                    , Point2 4 3
                                    , Point2 8 100
                                    , Point2 6 (-2)
                                    , Point2 10 2
                                    , Point2 11 3
                                    ]


-- | Naive 3-sided querying
naive   :: Ord r => (Range r, r) -> NonEmpty (Point 2 r :+ p) -> [Point 2 r :+ p]
naive q = List.sortBy (comparing f) . NonEmpty.filter (`in3Range` q)
  where
    f p = Down $ p^.core.yCoord

-- | test if a point lies in a 3sided range.
in3Range            :: Ord r => Point 2 r :+ p -> (Range r, r) -> Bool
p `in3Range` (r, y) = (p^.core.xCoord) `inRange` r && p^.core.yCoord >= y
