module CCWSpec
  ( spec
  ) where

import R
import HGeometry.Miso.Svg.StaticCanvas
import HGeometry.Miso.OrphanInstances ()
import System.OsPath
import HGeometry
import Golden
import Test.Hspec
import Test.Hspec.WithTempFile
import Control.Lens
import HGeometry.Miso.Svg.Draw
import Ipe.Color
import Ipe.Attributes

--------------------------------------------------------------------------------

spec :: Spec
spec = goldenWith [osp|docs/doc-figures/|]
         (svgFileGolden { name = [osp|ccw|] })
           (staticCanvas_ canvas [] $ mconcat
               [ draw @SVG [ stroke ?~ lightgray
                           ] (ClosedLineSegment origin (Point2 80 (80 :: R)))
               , draw @SVG [ fill ?~ purple ] p
               , draw @SVG [ fill ?~ blue   ] q
               , mconcat [draw @SVG [ fill ?~ red ] rCCw     | rCCw   <- rCCws]
               , mconcat [draw @SVG [ fill ?~ black ] rColin | rColin <- rColins]
               , mconcat [draw @SVG [ fill ?~ green ] rCw    | rCw    <- rCws ]
               ]
           )

p,q :: Point 2 R
p = Point2 5 5
q = Point2 20 20

rCCws, rColins, rCws :: [Point 2 R]
rCCws   = [Point2 20 30, Point2 5 50]
rColins = [Point2 10 10, Point2 70 70]
rCws    = [Point2 30 5, Point2 60 25]

canvas :: StaticCanvas R
canvas = staticCanvas 100 80
