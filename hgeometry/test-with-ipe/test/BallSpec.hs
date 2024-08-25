{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module BallSpec where

import Control.Lens
import Control.Monad ((>=>))
import Data.Maybe
import Golden
import HGeometry.Ball
import HGeometry.Boundary
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Vector
import Ipe
import Ipe.Color
import Paths_hgeometry
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
             [ map iO' lines'
             , map iO' balls
             , map (\case
                   Line_x_Ball_Point q     -> iO' q
                   Line_x_Ball_Segment seg -> iO $ defIO seg
                                                 ! attr SPen (IpePen "fat")
              ) intersections
             ])

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

intersections = catMaybes
                [ l `intersect` b
                | l <- lines', b <- balls
                ]
