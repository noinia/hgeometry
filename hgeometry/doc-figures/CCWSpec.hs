module CCWSpec
  ( spec
  ) where

import HGeometry.Ext
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
         (svgFileGolden { name = [osp|ccw|] })
           (staticCanvas_ canvas [] $ mconcat
               [ doubleBorderedPolygon myPolygon
               , draw @SVG [ fill ?~ purple ] p
               , draw @SVG [ fill ?~ blue ] q
               , mconcat [draw @SVG [ fill ?~ red ] rCCw     | rCCw   <- rCCws]
               , mconcat [draw @SVG [ fill ?~ black ] rColin | rColin <- rColins]
               , mconcat [draw @SVG [ fill ?~ green ] rCw    | rCw    <- rCws ]
               ]
           )

p,q :: Point 2 R
p = Point2 1 1
q = Point2 10 10

rCCws, rColins, rCws :: [Point 2 R]
rCCws   = [Point2 20 30, Point2 0 5]
rColins = [Point2 5 5, Point2 30 30]
rCws    = [Point2 10 5, Point2 30 0]

canvas :: StaticCanvas R
canvas = staticCanvas 100 500
