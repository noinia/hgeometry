module IntSpec where

import HGeometry.Vector.Vector2
-- import HGeometry.Vector
import Test.Hspec

--------------------------------------------------------------------------------

myVec :: Vector
myVec = Vector2 5 11

spec :: Spec
spec = describe "Int vector spec" $ do
         it "showtest" $
           show myVec `shouldBe` "Vector2 5 11"
         -- it "division" $
         --   myVec ^/ 2 `shouldBe` Vector2 2 5 -- this should not compile :)
