{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.BinaryTree.BalancedDecompositionSpec where

import           Algorithms.BinaryTree.BalancedDecomposition
import           Data.BinaryTree
import qualified Data.Foldable as F
import qualified Data.Set as S
import           Data.Util
import           QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

sizesOk               :: BinaryTree t -> Bool
sizesOk t | n < 3     = True
          | otherwise = f l && f r
  where
    Split (SP _ l) (SP _ r) = balancedSplit t
    n   = F.length t
    f x = n `div` 3 <= x && x <= (2*n) `div` 3

spec :: Spec
spec = do
  describe "BalancedSplit test" $ do
    it "sizes ok" $
      property $ \(t :: BinaryTree Int) -> sizesOk t `shouldBe` True
    it "same elements" $
      property $ \(t :: BinaryTree Int) ->
                     let Split (SP l _) (SP r _) = balancedSplit t
                         f = S.fromList . F.toList
                     in f t `shouldBe` (f l `S.union` f r)
