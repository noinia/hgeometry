module Algorithms.LogarithmicMethodSpec where

import           Algorithms.LogarithmicMethod
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.List.Util
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


newtype DummySucc a = Dummy (NonEmpty a)
  deriving (Show,Eq,Functor,Foldable,Foldable1,Traversable)

instance Ord a => LogarithmicMethodDS DummySucc a where
  build = Dummy . NonEmpty.sort

successor'              :: Ord a => a -> DummySucc a -> Option (Min a)
successor' q (Dummy xs) = case NonEmpty.dropWhile (< q) xs of
                            []    -> Option Nothing
                            (s:_) -> Option (Just (Min s))

successor   :: Ord a => a -> InsertionOnly DummySucc a -> Maybe a
successor q = fmap getMin . getOption . queryWith (successor' q)

fromList :: Ord a => [a] -> InsertionOnly DummySucc a
fromList = foldr insert empty
