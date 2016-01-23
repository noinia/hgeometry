module Data.Geometry.PointSpec where

import Data.Ext
import Data.Geometry.Point
import Test.Hspec

spec :: Spec
spec = do
  describe "Sort Arround a Point test" $ do
    it "Sort around origin" $ do
      sortArround (only origin) (map only [ point2 (-3) (-3)
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
      `shouldBe` map only [ point2 20   0
                          , point2 10   1
                          , point2 2    2
                          , point2 5    5
                          , point2 5    5
                          , point2 5    7
                          , point2 (-5) 3
                          , point2 (-3) (-3)
                          , point2 (-1) (-5)
                          , point2 6    (-4)
                          , point2 0    (-5)
                          , point2 0    (-6)
                          , point2 26   (-2)
                          ]
