module Data.Geometry.RangeTreeSpec where

import           Control.Lens
import           Control.Monad (forM_)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.PrioritySearchTreeSpec (Unique(..))
import           Data.Geometry.RangeTree
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Range
import qualified Data.Set as Set
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


spec :: Spec
spec = do
  describe "Range Search Tree" $ do
    it "reporting tests; same as naive" $ do
      let t  = createTree (Report . NonEmpty.toList) pts
          qs = [ (ClosedRange 1 10)
               , (ClosedRange 1 11)
               ]
      forM_ qs $ \q ->
        report q t `shouldBe` naive q pts
    it "quickcheck: same elems as naive" $
      property $ \(Unique qPts :: Unique (Int :+ Int)) q -> do
        Set.fromList (report q $ createTree (Report . NonEmpty.toList) qPts)
          `shouldBe`
          Set.fromList (naive q qPts)

pts :: NonEmpty (Int :+ Int)
pts = NonEmpty.fromList . map (\x -> x :+ x) $ [0..20]

--------------------------------------------------------------------------------

naive    :: Ord r => Range r -> NonEmpty (r :+ p) -> [p]
naive qr = map (^.extra) . NonEmpty.filter (\p -> (p^.core) `inRange` qr)
