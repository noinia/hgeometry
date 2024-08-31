{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.VisibilitySpec where

import Control.Lens
import Data.Maybe
import Golden
import HGeometry.Ball
import HGeometry.Combinatorial.Util
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph.Class
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Visibility
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "visibility graph / visibility polygon" $ do
         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeContentGolden { name = [osp|visibility|] })
             (concat
             [ [iO $ defIO myPolygon ! attr SLayer "polygon"
               ]
             , [ iO $ defIO (drawVisibilityEdge e myPolygon) ! attr SStroke red
                                                             ! attr SLayer "visibilityGraph"

               | e <- visibilityGraph myPolygon
               ]
             ])

drawVisibilityEdge (Two u v) pg = ClosedLineSegment (pg^?!vertexAt u) (pg^?!vertexAt v)

-- | Naive algorithm to compute the visibilityGraph of a simple polygon
--
-- O(n^3)
visibilityGraph     :: ( SimplePolygon_ simplePolygon point r
                       , HasIntersectionWith point simplePolygon
                       , Ord r, Fractional r
                        )
                    => simplePolygon -> [Two (VertexIx simplePolygon)]
visibilityGraph pg  = (uncurry Two <$> pg^..outerBoundaryEdges.asIndex)
                      <> mapMaybe liesInsidePolygon candidateEdges
  where
    candidateEdges = visibilityGraph' (pg^..outerBoundaryEdgeSegments)
                                      ((\(i,v) -> v :+ i) <$> pg^..vertices.withIndex)
    liesInsidePolygon (Two (u :+ i) (v :+ j))
      | u .+^ ((v .-. u) ^/ 2) `intersects` pg = Just (Two i j)
      | otherwise                              = Nothing


-- | computes the edges of the visibility graph among the points
--
-- O(n^2m), where n is the number of vertices, m is the number of obstacle edges.
visibilityGraph'           :: ( Foldable f, Foldable g
                              , Point_ vertex 2 r
                              , OpenLineSegment vertex `HasIntersectionWith` obstacleEdge
                              )  => f obstacleEdge -> g vertex -> [Two vertex]
visibilityGraph' obstacles = filter areMutuallyVisible . uniquePairs
  where
    areMutuallyVisible (Two u v) =
      all (not . (intersects (OpenLineSegment u v))) obstacles


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
