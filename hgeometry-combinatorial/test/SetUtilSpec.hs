module SetUtilSpec
  ( spec
  ) where


import           Data.Set (Set)
-- import qualified Data.Set as Set
import           HGeometry.Set.Util
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Set Util  tests" $ do
    describe "splitBy" $ do
      prop "splitBy satisfies properties, regular cmp " $
        \(q :: Int) set -> let f          = flip compare q
                               (lt,eq,gt) = splitBy f set
                           in and [ all (\x -> f x == LT) lt
                                  , all (\x -> f x == EQ) eq
                                  , all (\x -> f x == GT) gt
                                  ]
      prop "splitOn satisfies properties, regular cmp " $
        \(x :: Int) (set :: Set Int) -> let f y        = y + 10
                                            (lt,eq,gt) = splitOn f x set
                                        in and [ all (\y -> f y <  x) lt
                                               , all (\y -> f y == x) eq
                                               , all (\y -> f y >  x) gt
                                               ]
