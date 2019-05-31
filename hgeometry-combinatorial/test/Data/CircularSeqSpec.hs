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
      (c2 `isShiftOf` c1) `shouldBe` True
    it "is not a shift of " $ do
      let c1 :: CSeq Int
          c1 = fromList [1, 2, 3, 4]
          c2 = fromList [3, 2]
      (c1 `isShiftOf` c2) `shouldBe` False
      (c2 `isShiftOf` c1) `shouldBe` False
    it "multiple copies is not a shift" $ do
      let c1 = fromList [1]
          c2 = fromList [1,1]
          c3 = fromList [1,1,1]
      (c1 `isShiftOf` c2) `shouldBe` False
      (c2 `isShiftOf` c1) `shouldBe` False
      (c1 `isShiftOf` c3) `shouldBe` False
      (c3 `isShiftOf` c1) `shouldBe` False
    it "cyclic shift tests" $
      property $ \(xs :: NonEmpty Int) i -> do
                   let cs  = fromNonEmpty xs
                       cs' = rotateNR i cs
                   (cs `isShiftOf` cs') `shouldBe` True
                   (cs `isShiftOf` cs') `shouldBe` (isShiftOfNaive cs cs')
      -- property $ \(xs :: NonEmpty Int) i ->
      --              let cs  = fromNonEmpty xs
      --                  cs' = rotateNR i cs
      --              in



    -- it "cyclic shift is symmetric" $
    --   property $ \(xs :: NonEmpty Int) i ->
    --                let cs  = fromNonEmpty xs
    --                    cs' = rotateNR i cs
    --                in (cs `isShiftOf` cs') `shouldBe` (cs' `isShiftOf` cs)


isShiftOfNaive       :: Eq a => CSeq a -> CSeq a -> Bool
isShiftOfNaive xs ys = xs `elem` allRotations ys
