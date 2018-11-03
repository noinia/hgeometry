module Algorithms.Geometry.ClosestPair.ClosestPairSpec where

import qualified Algorithms.Geometry.ClosestPair.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ClosestPair.Naive as Naive
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import           Data.Util
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances ()
import           Test.QuickCheck.Instances ()


spec :: Spec
spec = do
    describe "ClosestPairSpec Algorithms" $ do
      modifyMaxSuccess (const 1000) $
        it "Naive and DivideAnd Conquer report same closest pair distance" $
          property $ \pts ->
            (squaredEuclideanDist' $ Naive.closestPair pts)
            ==
            (squaredEuclideanDist' $ DivideAndConquer.closestPair pts)


squaredEuclideanDist'                         :: Two (Point 2 Rational :+ ()) -> Rational
squaredEuclideanDist' (Two (p :+ _) (q :+ _)) = squaredEuclideanDist p q


-- LSeq {toSeq = fromList [Point2 [(-33759522867779) % 7496802324005,10839434579010 % 8710408063243] :+ (),Point2 [27010230067287 % 7207136323822,(-3164483769031) % 742671498890] :+ (),Point2 [16411948329569 % 7616584565141,12511394381428 % 2834373835667] :+ (),Point2 [(-327606334581) % 728344280722,(-33692910597997) % 8003329261050] :+ (),Point2 [(-15920889254819) % 4416206444274,31639684978225 % 4753346825613] :+ ()]}
