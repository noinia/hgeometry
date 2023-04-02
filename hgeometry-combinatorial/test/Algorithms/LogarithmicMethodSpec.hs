module Algorithms.LogarithmicMethodSpec where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           HGeometry.Algorithms.LogarithmicMethod
import           HGeometry.List.Util
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Logarithmic method successor test" $ do
         it "successor test" $ property $
           \q (xs :: [Int]) ->
             successor q (fromList xs)
             `shouldBe`
             minimum1 [ x | x <- xs, x >= q]

         it "merge test" $ property $
           \q (xs :: [Int]) (ys :: [Int]) ->
             successor q ((fromList xs) <> (fromList ys))
             `shouldBe`
             successor q (fromList $ xs <> ys)


newtype DummySucc a = Dummy (NonEmpty a)
  deriving (Show,Eq,Functor,Foldable,Foldable1,Traversable)

instance Ord a => LogarithmicMethodDS DummySucc a where
  build = Dummy . NonEmpty.sort

successor'              :: Ord a => a -> DummySucc a -> Maybe (Min a)
successor' q (Dummy xs) = case NonEmpty.dropWhile (< q) xs of
                            []    -> Nothing
                            (s:_) -> Just (Min s)

successor   :: Ord a => a -> InsertionOnly DummySucc a -> Maybe a
successor q = fmap getMin . queryWith (successor' q)

fromList :: Ord a => [a] -> InsertionOnly DummySucc a
fromList = foldr insert empty
