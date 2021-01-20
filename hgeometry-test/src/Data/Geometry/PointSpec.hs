module Data.Geometry.PointSpec (spec) where

import qualified Data.CircularList             as C
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Point.CmpAround
import           Data.Geometry.Vector
import           Data.Proxy
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances     ()
import           Test.Util
import Data.Double.Approximate (SafeDouble, DoubleRelAbs(..))
import Data.Coerce

--------------------------------------------------------------------------------

{-# NOINLINE double1 #-}
{-# NOINLINE double2 #-}
{-# NOINLINE double3 #-}
double1, double2 :: Double
double1 = 1e100
double2 = 1.1
double3 = 1.2

spec :: Spec
spec = do
  describe "Add vector to point" $ do
    it "2d" $
      origin .+^ Vector2 1 2 `shouldBe` Point2 1 2
    it "3d" $
      origin .+^ Vector3 1 2 3 `shouldBe` Point3 1 2 3

  describe "cmpAroundWith tests" $ do
    it "ccw same as by quarant " $
      property $ \(c :: Point 2 Int) (p :: Point 2 Int) (q :: Point 2 Int) ->
        (p /= c && q /= c) ==> ccwCmpAround c p q == ccwCmpAroundByQuadrant (ext c) (ext p) (ext q)
    it "cw same as by quarant " $
      property $ \(c :: Point 2 Int) (p :: Point 2 Int) (q :: Point 2 Int) ->
        (p /= c && q /= c) ==> cwCmpAround c p q == cwCmpAroundByQuadrant (ext c) (ext p) (ext q)

    it "cw same as by quarant with distance " $ do
      let cwCmpAroundWithDist c p q = cwCmpAround c p q <> cmpByDistanceTo c p q
      property $ \(c :: Point 2 Int) (p :: Point 2 Int) (q :: Point 2 Int) ->
        (p /= c && q /= c) ==>
        cwCmpAroundWithDist c p q == cwCmpAroundByQuadrantWithDist (ext c) (ext p) (ext q)

  describe "numerical robustness" $ do
    it "1e0" $
      ccw (Point2 0 0) (Point2 1 1) (Point2 2 (2::Double)) `shouldBe` CoLinear

    -- 'ccw' is doing this calculation:
    -- 1.2 * 1e100 * 1.1 -
    -- 1.1 * 1e100 * 1.2 =
    -- 0
    -- With infinite precision, everything should cancel out and the result is exactly 0.
    -- But Double doesn't have infinite precision so we end up with an error of 1 ULP
    -- for 1e100 which is a huge number 1.942668892225729e84. Obviously far greater than 0.
    -- We can get around this problem by using SafeDouble which considers doubles to be
    -- equal if they are within 10 ULPs.
    let doubleP1 = Point2 double3 double2
        doubleP2 = Point2 (double1*double3) (double1*double2)
    it "1e100" $
      ccw (Point2 0 0) doubleP1 doubleP2 `shouldNotBe` CoLinear
    it "1e100" $
      ccw (Point2 0 0) (coerce <$> doubleP1) (coerce <$> doubleP2 :: Point 2 SafeDouble)
        `shouldBe` CoLinear

  describe "Sort Arround a Point test" $ do
    it "Sort around origin" $
      sortAround origin [ Point2 (-3) (-3)
                        , Point2 (-1) (-5)
                        , Point2 5    5
                        , Point2 6    (-4)
                        , Point2 (-5) 3
                        , Point2 10   1
                        , Point2 20   0
                        , Point2 0    (-6)
                        , Point2 5    7
                        , Point2 5    5
                        , Point2 2    2
                        , Point2 26   (-2)
                        , Point2 0    (-5)
                         ]
      `shouldBe` [ Point2 20   0
                 , Point2 10   1
                 , Point2 2    2
                 , Point2 5    5
                 , Point2 5    5
                 , Point2 5    7
                 , Point2 (-5) 3
                 , Point2 (-3) (-3)
                 , Point2 (-1) (-5)
                 , Point2 0    (-5)
                 , Point2 0    (-6)
                 , Point2 6    (-4)
                 , Point2 26   (-2)
                 ]
    it "degenerate points on horizontal line" $
      sortAround origin [ Point2 2    0
                        , Point2 (-1) 0
                        , Point2 10   0
                        ]
      `shouldBe` [ Point2 2 0, Point2 10 0, Point2 (-1) 0 ]
    it "degenerate points on vertical line" $
      sortAround origin [ Point2 0 2
                        , Point2 0 (-1)
                        , Point2 0 10
                        ]
      `shouldBe` [ Point2 0 2, Point2 0 10, Point2 0 (-1) ]


  describe "Insert point in ciclically ordered list" $ do
    it "insert" $
      insertIntoCyclicOrder (ext origin) (ext $ Point2 (-4) (-5))  (
        C.fromList $ map ext [ Point2 20   0
                             , Point2 10   1
                             , Point2 2    2
                             , Point2 5    5
                             , Point2 5    5
                             , Point2 5    7
                             , Point2 (-5) 3
                             , Point2 (-3) (-3)
                             , Point2 (-1) (-5)
                             , Point2 0    (-5)
                             , Point2 0    (-6)
                             , Point2 6    (-4)
                             , Point2 26   (-2)
                             ])
      `shouldBe`
        (C.fromList $ map ext [ Point2 20   0
                              , Point2 10   1
                              , Point2 2    2
                              , Point2 5    5
                              , Point2 5    5
                              , Point2 5    7
                              , Point2 (-5) 3
                              , Point2 (-3) (-3)
                              , Point2 (-4) (-5)
                              , Point2 (-1) (-5)
                              , Point2 0    (-5)
                              , Point2 0    (-6)
                              , Point2 6    (-4)
                              , Point2 26   (-2)
                              ])

  specify "Read/Show properties for Point1" $
    property $ qcReadShow1 @(Point 1) Proxy
  specify "Read/Show properties for Point2" $
    property $ qcReadShow1 @(Point 2) Proxy
  specify "Read/Show properties for Point3" $
    property $ qcReadShow1 @(Point 3) Proxy
  specify "Read/Show properties for Point4" $
    property $ qcReadShow1 @(Point 4) Proxy

  specify "Read/Show properties for Vector1" $
    property $ qcReadShow1 @(Vector 1) Proxy
  specify "Read/Show properties for Vector2" $
    property $ qcReadShow1 @(Vector 2) Proxy
  specify "Read/Show properties for Vector3" $
    property $ qcReadShow1 @(Vector 3) Proxy
  specify "Read/Show properties for Vector4" $
    property $ qcReadShow1 @(Vector 4) Proxy
