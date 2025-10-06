module CyclicSpec(spec) where

import           Control.Lens
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.NonEmpty as NV
import           HGeometry.Combinatorial.Instances ()
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
    let test = toNonEmptyOf (asFold1 withCyclicSuccessor) (NonEmpty.fromList "abcde")
    in test `shouldBe` NonEmpty.fromList [('a','b'), ('b','c'),('c','d'),('d','e'),('e','a')]
  it "predecessor" $
    let test = runIdentity $ withCyclicPredecessor pure (NonEmpty.fromList "abcde")
    in test `shouldBe` NonEmpty.fromList [('e','a'), ('a','b'),('b','c'),('c','d'),('d','e')]

  it "neighbours" $
    let test3 = runIdentity $ withCyclicNeighbours pure (NonEmpty.fromList "abcde")
    in test3 `shouldBe` NonEmpty.fromList
       [('a', V2 'e' 'b'), ('b',V2 'a' 'c'),('c',V2 'b' 'd'),('d',V2 'c' 'e'),('e',V2 'd' 'a')]

  -- it "neighoours with index" $
  --   let itest = toNonEmptyOf (asIFold1 withCyclicSuccessor.withIndex) (NonEmpty.fromList "abcde")
  --   in itest `shouldBe` NonEmpty.fromList
  --      [((0, V2 6 1), ('a', V2 'e' 'b'))]

-- | Convert a traversal into a fold.
asFold1   :: Traversal1 s t a b -> Fold1 s a
asFold1 t = \aFa -> phantom . t (phantom . aFa)

-- -- | Convert a traversal into a fold.
-- asIFold1   :: IndexedTraversal1 i s t a b -> IndexedFold1 i s a
-- asIFold1 t = \aFa -> phantom . t (phantom . aFa)


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
                  flatten (groupWith (even . fst) xs) `isShiftOf` xs
              prop "same as groupWithOrig" $
                \(xs :: Cyclic NonEmpty (Int,Char)) ->
                  let f = even . fst in groupWith f xs === groupWithOrig f xs

  where
    cyclic  = Cyclic . NonEmpty.fromList
    flatten = Cyclic . foldMap1 snd

-- | My original implmentation, somewhat poorly documented
groupWithOrig      :: (Foldable1 cyclic, Eq b)
                   => (a -> b) -> cyclic a -> Cyclic NonEmpty (b, NonEmpty a)
groupWithOrig f xs = Cyclic $ case foldrMap1 initialize compute xs of
    Left res      -> NonEmpty.singleton res
    Right ((x, first), res@((y, current) :| completed))
      | x == y    -> (x, first <> current) :| completed
      | otherwise -> (x, first) NonEmpty.<| res
  where
    -- we either have a Left (single run) or a Right (lastRun, otherRuns)
    -- once we detect that we have more than one run, any future values will just be
    -- Right's.
    initialize x = Left (f x, NonEmpty.singleton x)
    compute x = \case
        Left (y,current)
          | b == y    -> Left (y, x NonEmpty.<| current)
          | otherwise -> Right ((y,current), NonEmpty.singleton (b, NonEmpty.singleton x))
        Right (first, res@((y,current):|completed))
          | b == y    -> Right (first, (y, x NonEmpty.<| current) :| completed)
          | otherwise -> Right (first, (b, NonEmpty.singleton x) NonEmpty.<| res)
      where
        b = f x
