module Data.Ratio.GeneralizedSpec where

import qualified Data.Ratio as Ratio
import           Data.Ratio.Generalized
import           Test.Hspec
import           Test.QuickCheck
--------------------------------------------------------------------------------

type GRational = GRatio Integer

(%%)   :: Integer -> Integer -> Rational
a %% b = a Ratio.% b

fromRational' :: Rational -> GRational
fromRational' = fromRational


itOp       :: String -> (forall a. Num a => a -> a -> a) -> Spec
itOp str op = it str $ property $ \a b ->
      fromRational' (a `op` b) `shouldBe` fromRational' a `op` fromRational' b

spec :: Spec
spec = do
  describe "GRatio tests" $ do
    it "Eq" $ property $ \a b c d -> b /= 0 && d /= 0 ==>
      a %% b == c %% d `shouldBe` a % b == c % d
    it "Ord" $ property $ \a b c d -> b /= 0 && d /= 0 ==>
      (a %% b) `compare` (c %% d) `shouldBe` (a % b) `compare` (c % d)
    itOp "+" (+)
    itOp "-" (-)
    itOp "*" (*)
    it "/" $ property $ \a b -> b /= 0 ==>
      fromRational' (a / b) `shouldBe` fromRational' a / fromRational' b
