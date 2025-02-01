module ShuffleSpec
  ( spec
  )  where


import qualified Data.Set as Set
import           HGeometry.Permutation.Shuffle
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "shuffle tests" $ do
         describe "mutable version"
           prop "is permutation" $
             \(gen :: StdGen) (elems :: [Int]) ->
               (Set.fromList elems) === (foldMap Set.singleton $ shuffle gen elems)
         describe "pure version version"
           prop "is permut" $
             \(gen :: StdGen) (elems :: [Int]) ->
               (Set.fromList elems) === (foldMap Set.singleton $ shuffleSeq gen elems)
