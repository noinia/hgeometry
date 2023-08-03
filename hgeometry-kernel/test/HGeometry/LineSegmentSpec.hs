{-# LANGUAGE ScopedTypeVariables #-}
module HGeometry.LineSegmentSpec where

-- import Control.Lens
import Data.Ord (comparing)
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
-- import Data.Vinyl
-- import HGeometry.Boundary
-- import HGeometry.Box
-- import HGeometry.Line
import HGeometry.LineSegment
-- import HGeometry.LineSegment.Internal (onSegment, onSegment2)
import HGeometry.Point
import HGeometry.Interval
import HGeometry.Vector ()
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck ((===), Arbitrary(..), suchThat)
import Test.QuickCheck.Instances ()
import HGeometry.Kernel.Instances ()

--------------------------------------------------------------------------------
-- main :: IO ()
-- main = print $ testStartInt

type R = RealNumber 5

pt :: Point 2 Double
pt = Point2 5 6

testInt :: ClosedInterval (Point 2 Double)
testInt = ClosedInterval pt (Point2 10 10)

data OrdAtXTest r = OrdAtXTest r (ClosedLineSegment (Point 2 r))
                                 (ClosedLineSegment (Point 2 r))
                  deriving (Show,Eq)
data OrdAtYTest r = OrdAtYTest r (ClosedLineSegment (Point 2 r))
                                 (ClosedLineSegment (Point 2 r))
                  deriving (Show,Eq)

instance (Arbitrary r, Ord r) => Arbitrary (OrdAtXTest r) where
  arbitrary = do x    <- arbitrary
                 seg1 <- intersectingSeg x
                 seg2 <- intersectingSeg x
                 pure $ OrdAtXTest x seg1 seg2
    where
      intersectingSeg x = do px      <- arbitrary `suchThat` (<= x)
                             qx      <- arbitrary `suchThat` (>= x)
                             (py,qy) <- arbitrary
                             b       <- arbitrary
                             let p = Point2 px py
                                 q = Point2 qx qy
                             pure $ if b then ClosedLineSegment p q else ClosedLineSegment q p

instance (Arbitrary r, Ord r) => Arbitrary (OrdAtYTest r) where
  arbitrary = do y    <- arbitrary
                 seg1 <- intersectingSeg y
                 seg2 <- intersectingSeg y
                 pure $ OrdAtYTest y seg1 seg2
    where
      intersectingSeg y = do py      <- arbitrary `suchThat` (<= y)
                             qy      <- arbitrary `suchThat` (>= y)
                             (px,qx) <- arbitrary
                             b       <- arbitrary
                             let p = Point2 px py
                                 q = Point2 qx qy
                             pure $ if b then ClosedLineSegment p q else ClosedLineSegment q p

spec :: Spec
spec =
  describe "line segment tests" $ do
    it "point" $ show pt `shouldBe` "Point2 5.0 6.0"
    it "interval" $
      (show testInt) `shouldBe` "Interval (ClosedE (Point2 5.0 6.0)) (ClosedE (Point2 10.0 10.0))"
    it "show segment" $
      (show $ ClosedLineSegment (Point2 5.0 6.0) (Point2 10.0 (10.0 :: Double)))
      `shouldBe`
      "ClosedLineSegment (Point2 5.0 6.0) (Point2 10.0 10.0)"

    prop "ordAtX same as ordAtX Fractional" $
      \(OrdAtXTest (x :: R) seg1 seg2) ->
        ordAtX x seg1 seg2 === ordAtXFractional x seg1 seg2
    prop "ordAtY same as ordAtY Fractional" $
      \(OrdAtYTest (y :: R) seg1 seg2) ->
        ordAtY y seg1 seg2 === ordAtYFractional y seg1 seg2

    describe "onSegment" $ do
      let intersects3 :: Point 3 R -> ClosedLineSegment (Point 3 R :+ ()) -> Bool
          intersects3 = intersects
          intersects2 :: Point 2 R -> ClosedLineSegment (Point 2 R :+ ()) -> Bool
          intersects2 = intersects

      it "2d on segment tests" $ do
        let seg1 = ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ())
            seg2 = ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ())
        (Point2 1    0 `intersects2` seg1) `shouldBe`  True
        (Point2 1    1 `intersects2` seg1) `shouldBe` False
        (Point2 5    0 `intersects2` seg1) `shouldBe` False
        (Point2 (-1) 0 `intersects2` seg1) `shouldBe` False
        (Point2 1    1 `intersects2` seg2) `shouldBe`  True

      it "3d on segment tests" $ do
        let seg = ClosedLineSegment (origin :+ ()) (Point3 3 3 3 :+ ())
        (Point3 1 1 1 `intersects3` seg) `shouldBe` True
        (Point3 1 2 1 `intersects3` seg) `shouldBe` False

      -- prop "onSegment2 same result as generic onSegment (quickheck; (mostly) false points)" $
      --   \(q :: Point 2 R) (seg :: ClosedLineSegment (Point 2 R)) ->
      --     q `onSegment` seg == q `onSegment2` seg
      --     -- note: most of the points above will likely not lie on the segment
      -- prop "onSegment2 same result as generic onSegment (quickheck ; true points)" $
      --   \(lambda :: R) (seg :: ClosedLineSegment (Point 2 R)) ->
      --     let v = (seg^.end.core) .-. (seg^.start.core)
      --         q = (seg^.start.core) .+^ (lambda *^ v)
      --     in q `onSegment` seg == q `onSegment2` seg

    testI

--     it "intersecting line segment and line" $ do
--       let s = ClosedLineSegment (ext origin) (ext $ Point2 10 (0 :: R))
--       (s `intersect` horizontalLine (0 :: R)) `shouldBe` coRec s


test1 :: ClosedLineSegment (Point 2 Int)
test1 = ClosedLineSegment (Point2 0 10) (Point2 0 20)

test2 :: OpenLineSegment (Point 2 Int)
test2 = OpenLineSegment (Point2 0 5) (Point2 0 20)

test3 :: ClosedLineSegment (Point 2 Int)
test3 = ClosedLineSegment (Point2 0 21) (Point2 0 5)

test4 :: LineSegment AnEndPoint (Point 2 Int)
test4 = LineSegment (AnEndPoint Open $ Point2 0 10) (AnEndPoint Closed $ Point2 0 9)

-- -- test = withRank (Vector2 0 1) test1 test4

testI :: Spec
testI = describe "some manual intersection tests" $ do
          pure ()
--           it "manual intersection" $ (test1 `intersects` test2 ) `shouldBe` True
--          it "manual intersection" $ (test1 `intersects` test3 ) `shouldBe` True
--           it "manual intersection" $ (test1 `intersects` test4 ) `shouldBe` False
--           it "manual intersection" $ (test2 `intersects` test4 ) `shouldBe` True




-- | Given an x-coordinate, compare the segments based on the
-- y-coordinate of the intersection with the horizontal line through y
ordAtXFractional   :: ( Fractional r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
                   => r
                   -> lineSegment -> lineSegment -> Ordering
ordAtXFractional x = comparing (yCoordAt x)

-- | Given an x-coordinate, compare the segments based on the
-- y-coordinate of the intersection with the horizontal line through y
ordAtYFractional   :: ( Fractional r, Ord r, LineSegment_ lineSegment point, Point_ point 2 r)
                   => r
                   -> lineSegment -> lineSegment -> Ordering
ordAtYFractional y = comparing (xCoordAt y)
