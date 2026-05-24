{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module SPTSpec
  ( spec
  ) where

import HGeometry.Ext
import Hiraffe.PlanarGraph.Connected
import R
import HGeometry.PlaneGraph
import HGeometry.Polygon.Triangulation
import HGeometry.Polygon.Simple.ShortestPath.Tree
import HGeometry.Miso.Svg.StaticCanvas
import HGeometry.Miso.OrphanInstances ()
import System.OsPath
import HGeometry
import HGeometry.Polygon
import Golden
import Test.Hspec
import Test.Hspec.WithTempFile
import Data.Maybe (fromJust)
import Control.Lens
import HGeometry.Miso.Svg.Draw
import Ipe.Color
import Ipe (IpeValue(..))
import Ipe.Attributes
import Miso.String (ToMisoString(..))

--------------------------------------------------------------------------------

spec :: Spec
spec = goldenWith [osp|docs/doc-figures/|]
         (svgFileGolden { name = [osp|shortestPathTree|] })
           (staticCanvas_ canvas [] $ mconcat
               [ doubleBorderedPolygon myPolygon
               , mconcat
                 [ draw @SVG [ stroke ?~ lightgray ] seg
                 | (e, Diagonal) <- triangulated^..edges.withIndex
                 , seg <- triangulated^..edgeSegmentAt e
                 ]
               , mconcat
                 [ draw @SVG [ stroke ?~ green
                             -- , arrow ?~ normalArrow
                             ] (sptEdge v parent)
                 | (v :+ parent) <- triangulated^..vertices
                 ]
               , draw @SVG
                      [fill ?~ blue] source
               ]
           )

-- | Draws the polygon using a nice double border
doubleBorderedPolygon      :: (Point_ vertex 2 r, ToMisoString r, Num r)
                           => SimplePolygon vertex
                           -> Rendered SVG
doubleBorderedPolygon poly = mconcat
    [ draw @SVG [ stroke   ?~ gray
                , pen      ?~ IpePen (Valued 7)
                ] poly
    , draw @SVG [ stroke ?~ black
                , fill ?~ white
                , pen    ?~ IpePen (Valued 2)
                ] poly
    ]

-- | Draws an spt edge
sptEdge   :: Point 2 R -> Either (Point 2 R) (VertexId S) -> ClosedLineSegment (Point 2 R)
sptEdge v = \case
  Left s       -> ClosedLineSegment v s
  Right parent -> ClosedLineSegment v (triangulated^?!vertexAt parent.core)

--------------------------------------------------------------------------------


type data S

-- | The triangulated polygon whose vertices are annotated with shortest paths
triangulated :: CPlaneGraph S (Point 2 R :+ Either (Point 2 R) (VertexId S))
                              PolygonEdgeType PolygonFaceData
triangulated = labelWithShortestPaths source $ triangulate myPolygon

source :: Point 2 R
source = Point2 224 48

myPolygon :: SimplePolygon (Point 2 R)
myPolygon = fromJust $ fromPoints
            [ Point2 64 176
            , Point2 160 144
            , Point2 176 208
            , Point2 80 240
            , Point2 128 256
            , Point2 240 224
            , Point2 224 160
            , Point2 288 128
            , Point2 336 208
            , Point2 416 80
            , Point2 352 32
            , Point2 336 96
            , Point2 272 32
            , Point2 192 32
            , Point2 224 96
            , Point2 176 112
            , Point2 128 64
            , Point2 144 32
            , Point2 160 80
            , Point2 192 80
            , Point2 144 6
            , Point2 48 96
            , Point2 112 112
            , Point2 96 144
            , Point2 64 112
            , Point2 16 112
            , Point2 48 144
            , Point2 32 176
            , Point2 32 224
            , Point2 64 224
            , Point2 96 208
            , Point2 128 176
            , Point2 80 192
            ]

canvas :: StaticCanvas R
canvas = staticCanvas 600 400


--------------------------------------------------------------------------------
