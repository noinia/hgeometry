module ShuffleSpec
  ( spec
  )  where

import           Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Vector (Vector)
import           HGeometry.Permutation.Shuffle
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ((===))

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "shuffle tests" $ do
         describe "mutable version" $ do
           prop "is permutation" $
             \gen (elems :: [Int]) ->
               (foldMap Set.singleton $ shuffle @Vector (mkStdGen gen) elems)
               ===
               (Set.fromList elems)

         describe "pure version version" $ do
           prop "is permut" $
             \gen (elems :: [Int]) ->
               (foldMap Set.singleton $ shuffleSeq (mkStdGen gen) elems)
               ===
               (Set.fromList elems)
           prop "identical to vec" $
             \gen (elems :: [Int]) ->
               (shuffleSeq (mkStdGen gen) elems)
               ===
               (foldMap Seq.singleton $ shuffle @Vector (mkStdGen gen) elems)


         describe "Intmap version version" $ do
           prop "is permut" $
             \gen (elems :: [Int]) ->
               (foldMap Set.singleton $ shuffleIntMap (mkStdGen gen) elems)
               ===
               (Set.fromList elems)
           prop "identical to vec" $
             \gen (elems :: [Int]) ->
               (toList $ shuffleIntMap (mkStdGen gen) elems)
               ===
               (toList $ shuffle @Vector (mkStdGen gen) elems)




         describe "pure version version (inout)" $ do
           prop "is permut" $
             \gen (elems :: [Int]) ->
               (foldMap Set.singleton $ shuffleSeqInOut (mkStdGen gen) elems)
               ===
               (Set.fromList elems)
           prop "identical to orig inout" $
             \gen (elems :: [Int]) ->
               (shuffleSeqInOut (mkStdGen gen) elems)
               ===
               (shuffleSeqInOutOrig (mkStdGen gen) elems)
