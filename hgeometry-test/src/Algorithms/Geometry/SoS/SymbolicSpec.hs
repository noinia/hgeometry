module Algorithms.Geometry.SoS.SymbolicSpec where

import           Algorithms.Geometry.SoS.Symbolic
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

type Index = Int

spec :: Spec
spec = describe "Symbolic Tests" $
         it "Ord < " $ property $ \a b i j -> a < b ==>
           symbolic' a i < symbolic b j `shouldBe` True


symbolic' :: Integer -> Index -> Symbolic Index Integer
symbolic' = symbolic
