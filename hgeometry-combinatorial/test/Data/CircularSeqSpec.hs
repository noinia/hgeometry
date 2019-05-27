module Data.CircularSeqSpec where

import Data.CircularSeq
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Data.List.NonEmpty(NonEmpty)

spec :: Spec
spec = do
  describe "CircularCeq tests" $ do
    it "isShiftOf" $ do
      let c1 :: CSeq Int
          c1 = fromList [1, 2, 1, 3]
          c2 = rotateNL 2 c1
      (c1 `isShiftOf` c2) `shouldBe` True
    it "cyclic shift tests" $
      property $ \(xs :: NonEmpty Int) i ->
                   let cs  = fromNonEmpty xs
                       cs' = rotateNR i cs
                   in (cs `isShiftOf` cs') `shouldBe` True
