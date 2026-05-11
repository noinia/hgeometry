{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import R
import HGeometry.Polygon.Convex.MinkowskiSum
import HGeometry.Miso.Svg.StaticCanvas
import HGeometry.Miso.Svg
import HGeometry.Miso.OrphanInstances ()
import System.OsPath
import Miso.Svg.Element
import HGeometry.Point
import HGeometry.Polygon
import Miso.String (ToMisoString(..))

--------------------------------------------------------------------------------

instance ToMisoString R where
  toMisoString = toMisoString . realToFrac @_ @Double

main :: IO ()
main = renderSvgToFile [osp|minkowski.svg|]
         (svg_ []
               [ draw (Point2 50 (100 :: R)) []
               ]
         )
