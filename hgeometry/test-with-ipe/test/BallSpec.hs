{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module BallSpec where

import Control.Lens
import Data.Maybe
import Golden
import HGeometry.Ball
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Point
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = Double

spec :: Spec
spec = describe "ball intersection with line" $ do
         goldenWith [osp|data/test-with-ipe/golden/|]
           (ipeContentGolden { name = [osp|ball|] })
             (concat
             [ map (\l -> iO'' l $ attr SLayer "lines"
                   ) lines'
             , map (\hl -> iO'' hl $  attr SLayer "halfLines"
                                   <> attr SStroke blue
                   ) halfLines
             , map (\b -> iO'' b $ attr SLayer "balls"
                   ) balls
             , map (\case
                   Line_x_Ball_Point q     -> iO'' (q^.core)
                                              $ attr SLayer "LineXBall"
                   Line_x_Ball_Segment seg -> iO'' (view core <$> seg)
                                              $ attr SPen (IpePen "fat")
                                              <> attr SLayer "LineXBall"
              ) intersections
             , map (\case
                   Line_x_Ball_Point q     -> iO'' (q^.core)
                                              $ attr SLayer "HalfLineXBall"
                   Line_x_Ball_Segment seg -> iO'' (view core <$> seg)
                                              $ attr SPen (IpePen "fat")
                                              <> attr SLayer "HalfLineXBall"
              ) hlIntersections
             ])

halfLines :: [HalfLine (Point 2 R)]
halfLines = [ HalfLine (Point2 4 0)      (Vector2 1 (-2))
            , HalfLine (Point2 (-5) 10)      (Vector2 1 (-5))
            ]

lines' :: [LinePV 2 R]
lines' = [ LinePV (Point2 0 10)      (Vector2 1 2)
         , LinePV (Point2 (-100) 20) (Vector2 1 0)
         , LinePV (Point2 (-100) 8) (Vector2 1 0)
         ]

balls :: [Ball (Point 2 R)]
balls = [ Ball origin 100
        , Ball (Point2 20 28) 64
        , Ball (Point2 200 28) 256
        ]

intersections = catMaybes [ l `intersect` b | l <- lines', b <- balls ]

hlIntersections = catMaybes [ l `intersect` b | l <- halfLines, b <- balls ]
