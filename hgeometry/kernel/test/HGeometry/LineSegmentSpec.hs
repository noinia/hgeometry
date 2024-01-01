{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HGeometry.LineSegmentSpec where

import Control.Lens ((^.), IxValue)
import Data.Ord (comparing)
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
-- import HGeometry.Boundary
-- import HGeometry.Box
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Interval
import HGeometry.HyperPlane
import HGeometry.Vector ((*^), Vector(..))
import HGeometry.Line.PointAndVector
import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck ((===), Arbitrary(..), suchThat, Property, counterexample)
import Test.QuickCheck.Instances ()
import HGeometry.Kernel.Instances ()

import Debug.Trace
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
          intersects2' :: Point 2 R -> OpenLineSegment (Point 2 R :+ ()) -> Bool
          intersects2' = intersects

      it "2d on segment tests" $ do
        let seg1 = ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ())
            seg2 = ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ())
            seg3 = OpenLineSegment   (origin :+ ()) (Point2 3 3 :+ ())
        (Point2 1    0 `intersects2` seg1) `shouldBe`  True
        (Point2 1    1 `intersects2` seg1) `shouldBe` False
        (Point2 5    0 `intersects2` seg1) `shouldBe` False
        (Point2 (-1) 0 `intersects2` seg1) `shouldBe` False
        (Point2 1    1 `intersects2` seg2) `shouldBe`  True
        (Point2 1    1 `intersects2'` seg3) `shouldBe`  True
        (Point2 0    0 `intersects2'` seg3) `shouldBe`  False

      it "3d on segment tests" $ do
        let seg = ClosedLineSegment (origin :+ ()) (Point3 3 3 3 :+ ())
        (Point3 1 1 1 `intersects3` seg) `shouldBe` True
        (Point3 1 2 1 `intersects3` seg) `shouldBe` False

      describe "verifying that onSegment2 is the same asOnSegment" $ do
        prop "closed segment (quickheck; (mostly) false points)" $
          propOnClosedSegment2Consistent @R
          -- note: most of the points above will likely not lie on the segment
        prop "closed segments (quickheck ; true points)" $
          \(lambda :: R) (seg :: ClosedLineSegment (Point 2 R)) ->
          let v = (seg^.end) .-. (seg^.start)
              q = (seg^.start) .+^ (lambda *^ v)
          in propOnClosedSegment2Consistent @R q seg

        prop "open segment (quickheck; (mostly) false points)" $
          propOnOpenSegment2Consistent @R
          -- note: most of the points above will likely not lie on the segment
        prop "open segments (quickheck ; true points)" $
          \(lambda :: R) (seg :: OpenLineSegment (Point 2 R)) ->
          let v = (seg^.end) .-. (seg^.start)
              q = (seg^.start) .+^ (lambda *^ v)
          in counterexample (show q) $ propOnOpenSegment2Consistent @R q seg

        prop "mixed segment (quickheck; (mostly) false points)" $
          propOnSegment2Consistent @R
          -- note: most of the points above will likely not lie on the segment
        prop "mixded segments (quickheck ; true points)" $
          \(lambda :: R) (seg :: LineSegment AnEndPoint (Point 2 R)) ->
          let v = (seg^.end) .-. (seg^.start)
              q = (seg^.start) .+^ (lambda *^ v)
          in counterexample (show q) $ propOnSegment2Consistent @R q seg

    testI

--------------------------------------------------------------------------------
-- * Make sure our 2d specialized instance ist he same as the generic one.

-- To test so, we lift the query point and the 2d semgent into 3d, by just setting the
-- z-coordinate to zero so that we use the generic algorithm. The output should be
-- consistent.

-- just put the z coord on zero
liftPt              :: Num r => Point 2 r -> Point 3 r
liftPt (Point2 x y) =  Point3 x y 0

liftSeg     :: (Num r
               , IxValue (endPoint (Point 2 r)) ~ Point 2 r
               , Functor endPoint
               ) => LineSegment endPoint (Point 2 r) -> LineSegment endPoint (Point 3 r)
liftSeg seg = LineSegment (liftPt <$> seg^.startPoint)
                          (liftPt <$> seg^.endPoint)

  -- seg&allPoints %~ liftPt

propOnClosedSegment2Consistent       :: (Ord r, Fractional r)
                                     => Point 2 r -> ClosedLineSegment (Point 2 r) -> Property
propOnClosedSegment2Consistent q seg =
  onSegment q seg === onSegment (liftPt q) (liftSeg seg :: ClosedLineSegment _)

propOnOpenSegment2Consistent       :: (Ord r, Fractional r)
                                   => Point 2 r -> OpenLineSegment (Point 2 r) -> Property
propOnOpenSegment2Consistent q seg =
  onSegment q seg === onSegment (liftPt q) (liftSeg seg :: OpenLineSegment _)
  -- onSegment2 q seg == onOpenSegmentD q seg

propOnSegment2Consistent       :: (Ord r, Fractional r, Show r)
                               => Point 2 r -> LineSegment AnEndPoint (Point 2 r) -> Property
propOnSegment2Consistent q seg =
  onSegment q seg === onSegment (liftPt q) (liftSeg seg :: LineSegment AnEndPoint _)
  -- onSegment2 q seg == onSegmentD q seg

--------------------------------------------------------------------------------



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

test5 :: OpenLineSegment (Point 2 Int)
test5 = OpenLineSegment (Point2 0 20) (Point2 200 20)

-- -- test = withRank (Vector2 0 1) test1 test4

vertSeg :: OpenLineSegment (Point 2 Int)
vertSeg = OpenLineSegment (Point2 20 (-5)) (Point2 20 10)

testI :: Spec
testI = describe "some manual intersection tests" $ do
          it "manual intersection 12" $ (test1 `intersects` test2 ) `shouldBe` True
          it "manual intersection 13" $ (test1 `intersects` test3 ) `shouldBe` True
          it "manual intersection 14" $ (test1 `intersects` test4 ) `shouldBe` False
          it "manual intersection 24" $ (test2 `intersects` test4 ) `shouldBe` True
          it "manual intersection 25" $ (test2 `intersects` test5 ) `shouldBe` False

          it "open ended segments x vertical Line" $
            (supportingLine vertSeg `intersects` test2) `shouldBe` False

          it "open ended segments; open endpoint on segment" $
            (test2 `intersects` vertSeg) `shouldBe` False
          it "open ended segments; endpointtest" $
            (Point2 20 (0 :: Int) `onSegment` test2) `shouldBe` False


          describe "manual intersect with line" $ do
            let l = LinePV origin (Vector2 0 (1 :: Int))
            it "man" $ (l `intersects` test1) `shouldBe` True
            it "sideTest" $ traceShow (hyperPlaneEquation l) $
              (onSideTest (test1^.start) l) `shouldBe` EQ



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



mySeg :: LineSegment AnEndPoint (Point 2 R)
mySeg = LineSegment (AnEndPoint Open (Point2 0 0)) (AnEndPoint Closed (Point2 (-1) 0))

testQ = propOnSegment2Consistent' (Point2 0 (0 :: R)) mySeg

propOnSegment2Consistent'       :: (Ord r, Fractional r, Show r)
                               => Point 2 r -> LineSegment AnEndPoint (Point 2 r) -> (Bool, Bool)
propOnSegment2Consistent' q seg =
  (onSegment q seg, onSegment (liftPt q) (liftSeg seg :: LineSegment AnEndPoint _))
