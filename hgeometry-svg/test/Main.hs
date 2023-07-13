{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}
module Main (main) where

import qualified Data.Text.Lazy.IO as Text
import           HGeometry.Box
import           HGeometry.Miso.Svg
import           HGeometry.Point
import qualified Miso
import           Miso.Svg
import           System.OsPath
--------------------------------------------------------------------------------

main :: IO ()
main = renderSvgToFile [osp|/tmp/foo.svg|] myDrawing

myDrawing = svg_ [ height_ "600", width_ "800" ]
                 [ draw (Rectangle (Point2 10 20) (Point2 100 (200 :: Int)))
                        [stroke_ "green" ]
                 ]
