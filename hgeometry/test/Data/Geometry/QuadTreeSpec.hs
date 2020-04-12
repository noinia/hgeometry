{-# LANGUAGE TypeApplications #-}
module Data.Geometry.QuadTreeSpec where

import Control.Lens
import Test.Hspec
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Box.Sides
import Data.Geometry.QuadTree
import Data.Geometry.QuadTree.Quadrants
import Data.Geometry.QuadTree.Tree(Tree(..))
import Data.Geometry.QuadTree.Cell
import Data.Maybe(isJust)
import Data.RealNumber.Rational

--------------------------------------------------------------------------------
type R = RealNumber 5

spec :: Spec
spec = do
    describe "QuadTree" $ do
      relTests

relTests :: Spec
relTests = it "correct relation" $ do
             let (QuadTree _ t) = withCells $ completeTree (Cell 1 $ origin @2 @R)
                 (Node (_ :+ c) _) = t
                 (Quadrants nw ne se sw) = splitCell c
                 a `hasRelTo` b = isJust <$> (() :+ a) `relationTo` b
                 empty = pure False
             (ne `hasRelTo` nw) `shouldBe` (empty&east .~ True)
             (ne `hasRelTo` se) `shouldBe` (empty&north .~ True)
             (ne `hasRelTo` sw) `shouldBe` empty
             (nw `hasRelTo` ne) `shouldBe` (empty&west .~ True)
             (sw `hasRelTo` nw) `shouldBe` (empty&south .~ True)
