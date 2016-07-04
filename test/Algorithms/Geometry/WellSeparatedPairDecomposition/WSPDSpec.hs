module Algorithms.Geometry.WellSeparatedPairDecomposition.WSPDSpec where

import           Algorithms.Geometry.WellSeparatedPairDecomposition.Types
import           Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD
import           Control.Lens
import           Data.Ext
import           Data.Geometry
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Seq2 as S2
import qualified Data.Vector as V
import           Test.Hspec
import           Util

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  reIndexTest
  distributePointsTest

reIndexTest :: Spec
reIndexTest = describe "ReIndex tests" $ do
    it "simple input reordering " $ do
      reIndexPoints input `shouldBe` output
  where
    input = v2 (ptSeq [ origin :+ 1, point2 1 1 :+ 100, point2 5 5 :+ 101 ])
               (ptSeq [ point2 1 1 :+ 100, point2 5 5 :+ 101, origin :+ 1 ])
    output = v2 (ptSeq [ origin :+ 0, point2 1 1 :+ 1, point2 5 5 :+ 2 ])
                (ptSeq [ point2 1 1 :+ 1, point2 5 5 :+ 2, origin :+ 0 ])



distributePointsTest :: Spec
distributePointsTest = describe "DistributePoints tests" $ do
    it "distributePoints' on a single list " $ do
      distributePoints' 3 levels input `shouldBe` output
    it "distributePoints on multiple lists" $ do
      distributePoints 3 levels (v2 input input) `shouldBe` output'

  where
    levels = V.fromList [Just $ Level 0 (Just 2),Just $ Level 1 (Just 1), Nothing]
    input  = ptSeq [ origin :+ 0, point2 1 1 :+ 1, point2 2 2 :+ 2]
    output = V.fromList [ ptSeq [origin :+ 0]
                        , ptSeq [point2 1 1 :+ 1]
                        , ptSeq [point2 2 2 :+ 2]
                        ]
    output' = fmap (\pts -> v2 pts pts) output

    --     input = v2 (f [ origin :+ 1, point2 1 1 :+ 100, point2 5 5 :+ 101 ])
--                (f [ point2 1 1 :+ 100, point2 5 5 :+ 101, origin :+ 1 ])
--     output = v2 (f [ origin :+ 0, point2 1 1 :+ 1, point2 5 5 :+ 2 ])
--                 (f [ point2 1 1 :+ 1, point2 5 5 :+ 2, origin :+ 0 ])

--     f = S2.viewL1FromNonEmpty . NonEmpty.fromList . map (&extra %~ ext)

ptSeq = S2.viewL1FromNonEmpty . NonEmpty.fromList . map (&extra %~ ext)
