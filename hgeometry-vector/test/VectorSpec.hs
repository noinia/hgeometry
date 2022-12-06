module VectorSpec (spec) where

-- import Data.Double.Approximate (SafeDouble)
import Data.Proxy
import HGeometry.Vector
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import HGeometry.Vector.Instances ()
import Test.Util

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

  -- -- this ia a bit annoying since we don't have a Arbitrary1 instance anymore.
  -- specify "Read/Show properties for Vector1" $
  --   property $ qcReadShow1 @(Vector 1) Proxy
  -- specify "Read/Show properties for Vector2" $
  --   property $ qcReadShow1 @(Vector 2) Proxy
  -- specify "Read/Show properties for Vector3" $
  --   property $ qcReadShow1 @(Vector 3) Proxy
  -- specify "Read/Show properties for Vector4" $
  --   property $ qcReadShow1 @(Vector 4) Proxy
