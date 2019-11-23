module Data.Geometry.PointSpec where

import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Point.CmpAround
import Data.Geometry.Vector
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Test.Hspec
import qualified Data.CircularList as C

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Add vector to point" $ do
    it "2d" $
      origin .+^ Vector2 1 2 `shouldBe` Point2 1 2
    it "3d" $
      origin .+^ Vector3 1 2 3 `shouldBe` Point3 1 2 3

  describe "cmpAroundWith tests" $ do
    it "ccw same as by quarant " $
      property $ \(c :: Point 2 Int :+ ()) (p :: Point 2 Int :+ ()) (q :: Point 2 Int :+ ()) ->
        ccwCmpAround c p q == ccwCmpAroundByQuadrant c p q
    it "cw same as by quarant " $
      property $ \(c :: Point 2 Int :+ ()) (p :: Point 2 Int :+ ()) (q :: Point 2 Int :+ ()) ->
        cwCmpAround c p q == cwCmpAroundByQuadrant c p q

    it "cw same as by quarant with distance " $ do
      let cwCmpAroundWithDist c p q = cwCmpAround c p q <> cmpByDistanceTo c p q
      property $ \(c :: Point 2 Int :+ ()) (p :: Point 2 Int :+ ()) (q :: Point 2 Int :+ ()) ->
        cwCmpAroundWithDist c p q == cwCmpAroundByQuadrantWithDist c p q

  describe "Sort Arround a Point test" $ do
    it "Sort around origin" $
      sortArround (ext origin) (map ext [ point2 (-3) (-3)
                                        , point2 (-1) (-5)
                                        , point2 5    5
                                        , point2 6    (-4)
                                        , point2 (-5) 3
                                        , point2 10   1
                                        , point2 20   0
                                        , point2 0    (-6)
                                        , point2 5    7
                                        , point2 5    5
                                        , point2 2    2
                                        , point2 26   (-2)
                                        , point2 0    (-5)
                                        ])
      `shouldBe` map ext [ point2 20   0
                         , point2 10   1
                         , point2 2    2
                         , point2 5    5
                         , point2 5    5
                         , point2 5    7
                         , point2 (-5) 3
                         , point2 (-3) (-3)
                         , point2 (-1) (-5)
                         , point2 0    (-5)
                         , point2 0    (-6)
                         , point2 6    (-4)
                         , point2 26   (-2)
                         ]
    it "degenerate points on horizontal line" $
      sortArround (ext origin) (map ext [ point2 2    0
                                        , point2 (-1) 0
                                        , point2 10   0
                                        ])
      `shouldBe` map ext [ point2 2 0, point2 10 0, point2 (-1) 0 ]
    it "degenerate points on vertical line" $
      sortArround (ext origin) (map ext [ point2 0 2
                                        , point2 0 (-1)
                                        , point2 0 10
                                        ])
      `shouldBe` map ext [ point2 0 2, point2 0 10, point2 0 (-1) ]


  describe "Insert point in ciclically ordered list" $ do
    it "insert" $
      insertIntoCyclicOrder (ext origin) (ext $ point2 (-4) (-5))  (
        C.fromList $ map ext [ point2 20   0
                             , point2 10   1
                             , point2 2    2
                             , point2 5    5
                             , point2 5    5
                             , point2 5    7
                             , point2 (-5) 3
                             , point2 (-3) (-3)
                             , point2 (-1) (-5)
                             , point2 0    (-5)
                             , point2 0    (-6)
                             , point2 6    (-4)
                             , point2 26   (-2)
                             ])
      `shouldBe`
        (C.fromList $ map ext [ point2 20   0
                              , point2 10   1
                              , point2 2    2
                              , point2 5    5
                              , point2 5    5
                              , point2 5    7
                              , point2 (-5) 3
                              , point2 (-3) (-3)
                              , point2 (-4) (-5)
                              , point2 (-1) (-5)
                              , point2 0    (-5)
                              , point2 0    (-6)
                              , point2 6    (-4)
                              , point2 26   (-2)
                              ])
