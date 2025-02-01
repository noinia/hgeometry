module ShuffleSpec
  ( spec
  )  where


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
