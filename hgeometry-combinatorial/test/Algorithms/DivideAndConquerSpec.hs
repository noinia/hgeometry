module Algorithms.DivideAndConquerSpec where

import Algorithms.DivideAndConquer

import           Test.Hspec
import           Test.QuickCheck
import qualified Data.List as List

spec = describe "divide and conquer strategy tests" $ do
         it "mergeSort" $ property $
           \(xs :: [Int]) -> List.sort xs == mergeSort xs


newtype MergeSort a = MergeSort [a]

instance Ord a => Semigroup (MergeSort a) where
  (MergeSort l) <> (MergeSort r) = MergeSort $ merge l r
    where
      merge [] ys = ys
      merge xs [] = xs
      merge a@(x:xs) b@(y:ys) = case x `compare` y of
                              LT -> x : merge xs b
                              EQ -> x : y : merge xs ys
                              GT -> y : merge a ys

instance Ord a => Monoid (MergeSort a) where
  mempty = MergeSort []


mergeSort :: Ord a => [a] -> [a]
mergeSort = (\(MergeSort xs) -> xs) . divideAndConquer (\x -> MergeSort [x])
