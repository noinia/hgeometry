module Vector2Spec where

import Control.Lens
import HGeometry.Vector
import HGeometry.Vector.Vector2
import Test.Hspec

--------------------------------------------------------------------------------

myVec :: Vector
myVec = Vector2 5 11

spec :: Spec
spec = describe "vector2 tests" $ do
         it "components" $
           myVec^..components `shouldBe` [5,11]
         it "component" $ do
           myVec^.(component @0) `shouldBe` 5
           myVec^.(component @1) `shouldBe` 11
         it "xComponent" $
           myVec^.xComponent `shouldBe` 5
         it "yComponent" $
           myVec^.yComponent `shouldBe` 11
         it "add" $
           myVec ^+^ myVec `shouldBe` Vector2 10 22
