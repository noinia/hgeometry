module VectorSpec (spec) where

import Control.Lens
import Data.Semigroup
import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import HGeometry.Vector.Instances ()
-- import           Test.Util

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "numerical robustness" $ do
    it "1e0" $
      isScalarMultipleOf (Vector2 1 10) (Vector2 10 (10*10::Double)) `shouldBe` True
    -- With sufficiently large numbers, isScalarMultipleOf fails for Doubles.
    it "1e10 (fail)" $
      isScalarMultipleOf (Vector2 1 10) (Vector2 1e10 (1e10*10::Double)) `shouldBe` False
    -- SafeDouble should work better.
    -- it "1e10 (pass)" $
    --   isScalarMultipleOf (Vector2 1 10) (Vector2 1e10 (1e10*10::SafeDouble)) `shouldBe` True
    it "division" $
      Vector2 5 (11 :: Double) ^/ 2 `shouldBe` Vector2 2.5 5.5

    it "vecVec" $
      let vecVec :: Vector 2 (Vector 2 Int)
          vecVec = Vector2 (Vector2 5 10) (Vector2 20 40)
      in (vecVec&components %~ view xComponent)
         `shouldBe`
         Vector2 5 20

  prop "dot implemented as foldMapZip" $ \(u :: Vector 3 Int) v ->
      (getSum $ foldMapZip (\x x' -> Sum $ x * x') u v) == (u `dot` v)
  it "vectorFromList" $ do
    vectorFromList @(Vector 1 Int) [10]       `shouldBe` Just (Vector1 10)
    vectorFromList @(Vector 3 Int) [10,2,3]   `shouldBe` Just (Vector3 10 2 3)
    vectorFromList @(Vector 3 Int) [10,2,3,5] `shouldBe` Nothing
    vectorFromList @(Vector 3 Int) [10,2]     `shouldBe` Nothing

  -- ordTests
  showReadTests

showReadTests :: Spec
showReadTests = describe "show/read tests for" $ do
  describe "Vector1" $ do
    prop "Double"       $ \(v :: Vector 1 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: Vector 1 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: Vector 1 Rational)       -> (read . show) v == v
    prop "Vector 2 Int" $ \(v :: Vector 1 (Vector 2 Int)) -> (read . show) v == v

  describe "Vector2" $ do
    prop "Double"       $ \(v :: Vector 2 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: Vector 2 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: Vector 2 Rational)       -> (read . show) v == v
    prop "Vector 2 Int" $ \(v :: Vector 2 (Vector 2 Int)) -> (read . show) v == v

  describe "Vector3" $ do
    prop "Double"       $ \(v :: Vector 3 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: Vector 3 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: Vector 3 Rational)       -> (read . show) v == v
    prop "Vector 2 Int" $ \(v :: Vector 3 (Vector 2 Int)) -> (read . show) v == v

  describe "Vector4" $ do
    prop "Double"       $ \(v :: Vector 4 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: Vector 4 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: Vector 4 Rational)       -> (read . show) v == v
    prop "Vector 2 Int" $ \(v :: Vector 4 (Vector 2 Int)) -> (read . show) v == v

  -- describe "Vector6" $ do
  --   prop "Double"       $ \(v :: Vector 6 Double)         -> (read . show) v == v
  --   prop "Int"          $ \(v :: Vector 6 Int)            -> (read . show) v == v
  --   prop "Rational"     $ \(v :: Vector 6 Rational)       -> (read . show) v == v
  --   prop "Vector 2 Int" $ \(v :: Vector 6 (Vector 2 Int)) -> (read . show) v == v


-- ordTests :: Spec
-- ordTests = describe "Ord tests for" $ do
--   describe "Vector2" $ do
--     prop "Int"       $ \u (v :: Vector 2 Int) ->
--       (vectorFromVector u `compare` vectorFromVector @_ @(Boxed.Vector 2 Int) v) ==
--       (u `compare` v)
--   describe "Vector3" $ do
--     prop "Int"       $ \u (v :: Vector 3 Int) ->
--       (vectorFromVector u `compare` vectorFromVector @_ @(Boxed.Vector 3 Int) v) ==
--       (u `compare` v)
--   describe "Vector4" $ do
--     prop "Int"       $ \u (v :: Vector 4 Int) ->
--       (vectorFromVector u `compare` vectorFromVector @_ @(Boxed.Vector 4 Int) v) ==
--       (u `compare` v)
--   describe "Vector7" $ do
--     prop "Int"       $ \u (v :: Vector 7 Int) ->
--       (vectorFromVector u `compare` vectorFromVector @_ @(Boxed.Vector 7 Int) v) ==
--       (u `compare` v)


-- fromListTests :: Spec
-- fromListTests = describe "fromList tests for" $ do
--   describe "Vector2 " $ do
--     prop "Int" $ \(v :: Vector 2 Int) -> let v' = vectorFromVector @_ @(Boxed.Vector 2 Int) v in
--                  v^..components == v'^..components
--     -- prop "Show Int" $ \(v :: Vector 2 Int) -> let v'= v&components.traverse %~ show in
--     --              (show <$> v^..components) == v'^..components
--   describe "Vector3 " $ do
--     prop "Int" $ \(v :: Vector 3 Int) -> let v' = vectorFromVector @_ @(Boxed.Vector 3 Int) v in
--                  v^..components == v'^..components
--   describe "Vector4 " $ do
--     prop "Int" $ \(v :: Vector 4 Int) -> let v' = vectorFromVector @_ @(Boxed.Vector 4 Int) v in
--                  v^..components == v'^..components
--   describe "Vector5 " $ do
--     prop "Int" $ \(v :: Vector 5 Int) -> let v' = vectorFromVector @_ @(Boxed.Vector 5 Int) v in
--                  v^..components == v'^..components
