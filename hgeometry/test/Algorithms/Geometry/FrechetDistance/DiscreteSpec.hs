module Algorithms.Geometry.FrechetDistance.DiscreteSpec where

import Data.Ext
import Algorithms.Geometry.FrechetDistance.Discrete
import Data.Geometry.Point
import Test.Hspec


spec :: Spec
spec = do
         it "trivial" $ do
           let ta = map ext [origin, Point2 5 5]
               tb = map ext [Point2 0 1, Point2 5 6, Point2 6 6]
           discreteFrechetDistance ta tb `shouldBe` 2
         it "trivial 2" $ do
           let ta = map ext [Point2 x 0     | x <- [1..10]]
               tb = map ext [Point2 x (y x) | x <- [1..10]]
               y x = if x == 5 then 5 else 1
           discreteFrechetDistance ta tb `shouldBe` 25
