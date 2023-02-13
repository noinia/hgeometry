module Vector2Spec where

import Control.Lens
import HGeometry.Vector.Unpacked
import HGeometry.Vector.Class
import Test.Hspec
import R

--------------------------------------------------------------------------------

myVec :: Vector 2 R
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
         it "dot" $
           myVec `dot` Vector2 100 2 `shouldBe` 522
         it "vectorFromList" $ do
           vectorFromList @(Vector 3 R) [10,2,3]   `shouldBe` Just (Vector3 10 2 3)
           vectorFromList @(Vector 3 R) [10,2,3,5] `shouldBe` Nothing
           vectorFromList @(Vector 3 R) [10,2]     `shouldBe` Nothing
