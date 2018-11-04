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
        it "Naive and DivideAnd Conquer report same closest pair distance (quickcheck)" $
          property $ \pts ->
            (squaredEuclideanDist' $ Naive.closestPair pts)
            ==
            (squaredEuclideanDist' $ DivideAndConquer.closestPair pts)
      it "Naive and DivideAnd Conquer report same closest pair distance (manual)" $ do
        let myPts = toLSeq myTest in
          (squaredEuclideanDist' $ Naive.closestPair myPts)
          `shouldBe`
          (squaredEuclideanDist' $ DivideAndConquer.closestPair myPts)


squaredEuclideanDist'                         :: Two (Point 2 Rational :+ ()) -> Rational
squaredEuclideanDist' (Two (p :+ _) (q :+ _)) = squaredEuclideanDist p q


-- LSeq {toSeq = fromList [Point2 [(-33759522867779) % 7496802324005,10839434579010 % 8710408063243] :+ (),Point2 [27010230067287 % 7207136323822,(-3164483769031) % 742671498890] :+ (),Point2 [16411948329569 % 7616584565141,12511394381428 % 2834373835667] :+ (),Point2 [(-327606334581) % 728344280722,(-33692910597997) % 8003329261050] :+ (),Point2 [(-15920889254819) % 4416206444274,31639684978225 % 4753346825613] :+ ()]}


toLSeq :: [a] -> LSeq 2 a
toLSeq = LSeq.promise . LSeq.fromList

myTest :: [Point 2 Rational :+ ()]
myTest = read "[Point2 [146640303144371 % 4224053853937,101854287495663 % 611897639578] :+ (),Point2 [40638737917185 % 6880564569878,266207821342347 % 5620065708622] :+ (),Point2 [(-22768678067038) % 4099605651011,63425313194697 % 3004649322800] :+ (),Point2 [(-79043128684637) % 1637661769455,(-295977300701107) % 9093457570027] :+ (),Point2 [(-73583019410059) % 7397585905521,(-132085857579544) % 3023721689783] :+ (),Point2 [110730624564935 % 3206669900528,(-134126355694632) % 1030756818019] :+ (),Point2 [375473877556369 % 5688548958491,(-61990694642620) % 8334329062977] :+ (),Point2 [113637651255443 % 7393857491411,60345369766453 % 1970866530039] :+ (),Point2 [18099493254552 % 6747283329067,(-104898261130768) % 6685232742229] :+ (),Point2 [(-99452695817779) % 9671436420976,(-15569270478765) % 353842993324] :+ (),Point2 [(-307985949779841) % 8267832155219,(-104994723690859) % 937448083071] :+ (),Point2 [5298565527551 % 9166217911857,361269627209233 % 6403545389662] :+ (),Point2 [(-53286482779806) % 163082999159,32112688900059 % 718598733692] :+ (),Point2 [(-107690491383153) % 5350356516874,(-335465470420443) % 9259993302154] :+ (),Point2 [(-1431112842609) % 2908633300498,(-6394060822783) % 6674992423] :+ (),Point2 [(-297096490651238) % 8936478049895,88152403947309 % 6679373739130] :+ (),Point2 [(-69377170752544) % 423340544261,85249651585993 % 1566182413847] :+ (),Point2 [(-76831240905987) % 7713729889276,(-161201413608815) % 3632380150759] :+ (),Point2 [120510849091594 % 469904179619,(-172736649495640) % 5333925170989] :+ (),Point2 [(-188872871543069) % 1898068101049,355493261879401 % 7036186201752] :+ (),Point2 [(-255393995322931) % 1103177446757,265927198927640 % 3105872402029] :+ (),Point2 [(-69257424852911) % 283924670424,157163114450212 % 6947926872911] :+ (),Point2 [160793284019169 % 4243291855883,136543247038343 % 663996934927] :+ (),Point2 [349183720690751 % 69082861367,27899563589967 % 984451034746] :+ (),Point2 [(-6729975907016) % 287613226363,(-132751704193606) % 884101426259] :+ (),Point2 [158419738815649 % 1684813345364,(-232301201438133) % 2251322747338] :+ (),Point2 [53058623626229 % 834280423049,11416530634139 % 5498459429949] :+ (),Point2 [153130827172836 % 3179759716621,(-247386168772091) % 6720178879120] :+ (),Point2 [(-255140791023605) % 6181407399187,(-58852369239783) % 1447725941071] :+ (),Point2 [(-35688634701875) % 8985183678917,162916031022373 % 4757510120717] :+ ()]"
