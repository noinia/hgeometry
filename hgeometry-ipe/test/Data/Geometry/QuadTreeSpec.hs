module Data.Geometry.QuadTreeSpec where

import Test.Hspec
import Data.Ext
import Data.Geometry.QuadTree
import Data.Geometry.QuadTree.Quadrants
import Data.Geometry.QuadTree.Cell
import Data.ByteString(ByteString)
import Data.Proxy


spec :: Spec
spec = do
    describe "QuadTree" $ do
      relTests

relTests :: Spec
relTests = it "correct relation" $ do
             let qt@(QuadTree _ t) = withCells $ completeTree 1
                 (Node (_ :+ c) _) = t
                 (Quadrants nw ne se sw) = splitCell c
                 a `hasRelTo` b = (not . null) <$> (() :+ ne) `relationTo` nw
             (ne `hasRelTo` nw) `shouldBe` (mempty&east .~ True)
             (ne `hasRelTo` sw) `shouldBe` (mempty&north .~ True)
             (ne `hasRelTo` se) `shouldBe` mempty
             (nw `hasRelTo` ne) `shouldBe` (mempty&west .~ True)
             (sw `hasRelTo` nw) `shouldBe` (mempty&south .~ True)
