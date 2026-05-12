{-# LANGUAGE QuasiQuotes #-}
module Golden where

import qualified Paths_hgeometry as Paths
import           System.OsPath
import           Test.Hspec.WithTempFile
import           Miso
import           Miso.Html.Render
import           HGeometry.Miso.Svg.StaticCanvas
import           HGeometry.Miso.Svg
import           R
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as Char8

--------------------------------------------------------------------------------

instance Show (View model action) where
  show = Char8.unpack . toHtml

instance ToMisoString R where
  toMisoString = toMisoString . realToFrac @_ @Double

svgFileGolden :: Golden ByteString.ByteString (View model action)
svgFileGolden = byteStringGolden { actualWriter = WriteActual renderSvgToFile
                                 , goldenFile   = [osp|figures.svg|]
                                 }



-- main = renderSvgToFile [osp|minkowski.svg|]
--          (staticCanvas_ canvas []
--                [ draw (minkowskiSum basePolygon offsetPolygon) [fill_ "red", stroke_ "black"]
--                , draw basePolygon [fill_ "blue", stroke_ "black"]
--                , draw (translateBy (Vector2 40 40) offsetPolygon
--                       ) [fill_ "green", stroke_ "black"]
--                ]
--          )
