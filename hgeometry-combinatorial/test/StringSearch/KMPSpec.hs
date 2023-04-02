module Algorithms.StringSearch.KMPSpec where

import           Algorithms.StringSearch.KMP
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

patternFound :: String -> String -> Maybe Int -> Bool
patternFound p t = \case
                     Nothing -> True
                     Just i  -> List.isPrefixOf p . List.drop i $ t

spec :: Spec
spec = do
  describe "KMP tests" $ do
    it "failure-function manual example" $
      buildFailureFunction (V.fromList "abacab")
        `shouldBe` (UV.fromList [0,0,1,0,1,2])
    it "manual example" $
      [4,1,2,3] `isSubStringOf` [1,4,5,4,1,2,3,6]
        `shouldBe` (Just 3)
