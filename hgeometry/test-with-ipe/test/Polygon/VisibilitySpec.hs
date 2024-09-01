{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.VisibilitySpec where

import           Control.Lens
import           Data.Maybe
import qualified Data.Set as Set
import           Golden
import           HGeometry.Ball
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph.Class
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
-- import           HGeometry.Polygon.Visibility
import qualified HGeometry.Polygon.Visibility.Naive as Naive
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "visibility graph / visibility polygon" $ do
         -- prop "naive visibility graph and fast one the same " $
         --   \(pg :: SimplePolygon (Point 2 R)) ->
         --     Set.fromList (visibilityGraph pg) === Set.fromList (Naive.visibilityGraph pg)

         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeContentGolden { name = [osp|visibility|] })
             (concat
             [ [iO $ defIO myPolygon ! attr SLayer "polygon"
               ]
             , [ iO $ defIO (drawVisibilityEdge e myPolygon) ! attr SStroke red
                                                             ! attr SLayer "visibilityGraph"

               | e <- visibilityGraph myPolygon
               ]
             , [ iO $ defIO (drawVisibilityEdge e myPolygon) ! attr SStroke red
                                                             ! attr SLayer "naive"

               | e <- Naive.visibilityGraph myPolygon
               ]
             ])

drawVisibilityEdge (Vector2 u v) pg = ClosedLineSegment (pg^?!vertexAt u) (pg^?!vertexAt v)



myPolygon :: SimplePolygon (Point 2 R)
myPolygon = fromJust
          $ fromPoints [ origin
                       , Point2 100 100
                       , Point2 0 100
                       , Point2 (-20) 80
                       , Point2 (-20) 200
                       , Point2 (-30) 200
                       , Point2 (-30) (-20)
                       , Point2 (-25) 0
                       ]

-- out = pg
