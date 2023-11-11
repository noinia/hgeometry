{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}
module Main (main) where

import qualified Data.Text.Lazy.IO as Text
import           HGeometry.Box
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.StaticCanvas
import           HGeometry.Point
import           Ipe
import qualified Miso
import           Miso.Svg
import           System.OsPath
--------------------------------------------------------------------------------

main :: IO ()
main = renderSvgToFile [osp|/tmp/foo.svg|] myCanvasDrawing

myDrawing = svg_ [ height_ "600", width_ "800" ]
                 [ draw (Rectangle (Point2 10 20) (Point2 100 (200 :: Int)))
                        [stroke_ "green" ]
                 ]

myCanvas :: StaticCanvas Double
myCanvas = staticCanvas 800 600

myCanvasDrawing = staticCanvas_ myCanvas []
                  [ draw (Rectangle (Point2 10 20) (Point2 100 (200 :: Int)))
                         [stroke_ "green" ]
                  , draw (Label "test" (Point2 300 (200 :: Int)))
                         [stroke_ "blue"]
                  ]
