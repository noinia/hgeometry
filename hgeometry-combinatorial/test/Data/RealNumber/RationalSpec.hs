{-# LANGUAGE DataKinds #-}
module Data.RealNumber.RationalSpec where

import           Data.RealNumber.Rational
import           Test.Hspec
import           Test.QuickCheck

type R = RealNumber 5

spec :: Spec
spec = do
  describe "Read/Show" $ do
    it "read basic" $ do
      read (show (1::R)) `shouldBe` (1::R)
      read (show (negate 1::R)) `shouldBe` negate (1::R)
    it "read nested" $ do
      read (show (Just (1::R))) `shouldBe` Just (1::R)
      read (show (Just (negate 1::R))) `shouldBe` Just (negate (1::R))
    specify "significant digits" $ do
      show (0.12345::RealNumber 2) `shouldBe` "0.12~"
      show (0.12345::RealNumber 5) `shouldBe` "0.12345"
    it "should discard tail" $ do
      read (show (0.12345::RealNumber 2)) `shouldBe` (0.12 ::R)
      reads "0.12345~" `shouldBe` [(0.12::RealNumber 2, "")]

