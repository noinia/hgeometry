module Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemovalSpec where

import Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval
import Control.Lens
import Control.Monad (forM_)
import Data.Ext
import Data.Geometry.Arrangement
import Data.Geometry.Point
import Data.Geometry.Triangle
import Data.Ratio
import Test.Hspec

--------------------------------------------------------------------------------

mkTri         :: Point 2 r
              -> Point 2 r
              -> Point 2 r
              -> r
              -> Triangle 2 () r :+ (Triangle 3 () r :+ r)
mkTri p q r z = let t2 = triangle' p q r
                    l (Point2 x y) = Point3 x y z
                in t2 :+ (triangle' (l p) (l q) (l r) :+ z)

liftTests :: Spec
liftTests = describe "liftToR3 tests" $ do
              forM_ [(q,tri) | q <- pts, tri <- tris] $ \(q,tri) ->
                it "liftToR3" $ do
                  liftToR3 q tri `shouldBe` (l q $ tri^.extra.extra)
  where
    l (Point2 x y) z = Point3 x y z

    pts  = [Point2 1 1, Point2 3 2]
    tris :: [Tri () () Rational Rational]
    tris = [mkTri origin (Point2 10 0) (Point2 0 10) (3 % 1)]

spec :: Spec
spec = do
    describe "HiddenSurfaceremoval tests" $ do
      liftTests
