{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module SPTSpec
  ( spec


  , Svg
  ) where


import           HGeometry.Ext
import           Hiraffe.PlanarGraph.Connected
import           R
import           HGeometry.PlaneGraph
import           HGeometry.Polygon.Triangulation
import           HGeometry.Polygon.Simple.ShortestPath.Tree
import           HGeometry.Miso.Svg.StaticCanvas
import qualified HGeometry.Miso.Svg as Svg
import           HGeometry.Miso.OrphanInstances ()
import           System.OsPath
import           Miso.Svg.Property
import           Miso.String (ToMisoString(..))
import           HGeometry
import           HGeometry.Polygon
import           Golden
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Data.Maybe (fromJust)
import           Control.Lens
import           Data.Kind (Type)

import           HGeometry.PolyLine
import           HGeometry.LineSegment
import           Data.Default
import           Ipe.Attributes
import           Ipe.Draw
import           Ipe.Color
import           Ipe (IpePen(..), IpeValue(..))
import           Miso (View)
--------------------------------------------------------------------------------

spec :: Spec
spec = goldenWith [osp|docs/doc-figures/|]
         (svgFileGolden { name = [osp|shortestPathTree|] })
           (staticCanvas_ canvas [] $ mconcat
               [ draw @(Svg _ _)
                      [ stroke ?~ gray
                      , pen    ?~ IpePen (Valued 7)
                      ] myPolygon
               , draw @(Svg _ _)
                      [ stroke ?~ black
                      , fill ?~ white
                      , pen    ?~ IpePen (Valued 2)
                      ] myPolygon
               , draw @(Svg () ())
                      [fill ?~ blue] source
               ]
           )


type data S
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
            , Point2 144 0
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

-- | The Svg backend; which renders to Svg using Miso
type data Svg (model :: Type) (action :: Type)

type instance Rendered (Svg model action) = [View model action]

instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , SimplePolygon_ (SimplePolygonF f vertex) vertex (NumType vertex)
         ) => IsDrawable (Svg model action) (SimplePolygonF f vertex) where
  type AttrOf (Svg model action) (SimplePolygonF f vertex) = PathAttributes (NumType vertex)
  draw ats poly = [ Svg.dSimplePolygon poly (Svg.svgWriteAttrs $ apply ats)
                  ]

instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , PolyLine_ (PolyLineF f vertex) vertex
         ) => IsDrawable (Svg model action) (PolyLineF f vertex) where
  type AttrOf (Svg model action) (PolyLineF f vertex) = PathAttributes (NumType vertex)
  draw ats poly = [ Svg.dPolyLine poly (Svg.svgWriteAttrs $ apply ats) ]


instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , EndPoint_ (endPoint vertex), IxValue (endPoint vertex) ~ vertex
         ) => IsDrawable (Svg model action) (LineSegment endPoint vertex) where
  type AttrOf (Svg model action) (LineSegment endPoint vertex) = PathAttributes (NumType vertex)
  draw ats seg = [ Svg.dLineSegment seg (Svg.svgWriteAttrs $ apply ats) ]


instance ( ToMisoString r
         ) => IsDrawable (Svg model action) (Point 2 r) where
  type AttrOf (Svg model action) (Point 2 r) = SymbolAttributes r
  draw ats p = [ Svg.dPoint p (Svg.svgWriteAttrs $ apply ats)]

apply :: Default at => [at -> at] -> at
apply = foldl' (flip ($)) def
