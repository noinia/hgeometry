module CyclicSpec(spec) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.NonEmpty as NV
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import           HGeometry.Sequence.NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Cyclic tests" $ do
    describe "HasDirectedTraversals tests" $ do
      prop "traverseRight consistent between vector ans NonEmpty" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseRightFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseRightFrom i) xs
      prop "traverseRight consistent between vector ans NonEmpty" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseLeftFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseLeftFrom i) xs
      prop "traverseRight consistent between vector and NonEmpty Sequence" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseRightFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseRightFrom i) (fromNonEmpty @ViewL1 xs)
      prop "traverseRight consistent between vector and NonEmpty Sequence" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseLeftFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseLeftFrom i) (fromNonEmpty @ViewL1 xs)
    withNeighboursSpec
    groupSpec

withNeighboursSpec :: Spec
withNeighboursSpec = describe "withNeighbours" $ do
  it "successor" $
    let test = runIdentity $ withCyclicSuccessor pure (NonEmpty.fromList "abcde")
    in test `shouldBe` NonEmpty.fromList [('a','b')]
  it "predecessor" $
    let test = runIdentity $ withCyclicPredecessor pure (NonEmpty.fromList "abcde")
    in test `shouldBe` NonEmpty.fromList [('a','b')]
  it "neighbours" $
    let test3 = runIdentity $ withCyclicNeighbours pure (NonEmpty.fromList "abcde")
    in test3 `shouldBe` NonEmpty.fromList [('a', V2 'a' 'a')]


groupSpec :: Spec
groupSpec = describe "cyclic groupWith tests" $ do
              it "manual" $ do
                let
                  input = cyclic [0,2,1,2,2,3,3,4,4,5,5,5,5,66,666,00,2,10,1,20]
                  ans   = cyclic [ (True, NonEmpty.fromList [20,0,2])
                                 , (False, NonEmpty.fromList [1])
                                 , (True, NonEmpty.fromList [2,2])
                                 , (False, NonEmpty.fromList [3,3])
                                 , (True, NonEmpty.fromList [4,4])
                                 , (False, NonEmpty.fromList [5,5,5,5])
                                 , (True, NonEmpty.fromList [66,666,0,2,10])
                                 , (False, NonEmpty.fromList [1])
                                 ]
                groupWith even input `shouldBe` ans
              prop "sameOrder" $
                \(xs :: Cyclic NonEmpty (Int,Char)) ->
                  (flatten (groupWith (even . fst) xs)) `isShiftOf` xs

  where
    cyclic  = Cyclic . NonEmpty.fromList
    flatten = Cyclic . foldMap1 snd
