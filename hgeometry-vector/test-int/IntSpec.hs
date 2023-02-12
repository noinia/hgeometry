module IntSpec where

import HGeometry.Vector.Unpacked
import HGeometry.Vector.Class
import Test.Hspec
import R

--------------------------------------------------------------------------------

myVec :: Vector 2 R
myVec = Vector2 5 11

spec :: Spec
spec = describe "Int vector spec" $ do
         it "showtest" $
           show myVec `shouldBe` "Vector2 5 11"
         -- it "division" $
         --   myVec ^/ 2 `shouldBe` Vector2 2 5 -- this should not compile :)
