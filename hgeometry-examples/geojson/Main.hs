{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import           Data.Aeson
import           Data.Geospatial
import           Data.Maybe
import           HGeometry.GeoJSON
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
-- import           HGeometry.Vector ()
import           Ipe
import           Paths_hgeometry_examples
import qualified System.File.OsPath as File
import           System.OsPath
--------------------------------------------------------------------------------

type R = Double

parseGeoJSONFile :: OsPath -> IO (Either String (GeoFeatureCollection Value))
parseGeoJSONFile = fmap eitherDecode . File.readFile


-- toPolygon :: IpeOut GeoPolygon Ipe.Path R
-- toPolygon = ipePolygon . fromMaybe (error "failed") . toPolygon'

-- toPolygon' :: GeoPolygon -> Maybe (SimplePolygon (Point 2 R))
-- toPolygon' = fromPoints . view (vertices.asPoint)


main :: IO ()
main = do
  res <- parseGeoJSONFile [osp|data/ne_110m_admin_1_states_provinces_shp.geojson|]
  case res of
    Left err          -> print err
    Right fCollection -> do
      mapM_ print $ fCollection^..geofeatures.traverse.geometry._Polygon
      -- let outFp = [osp|foo.ipe|]
      --     out   = [ iO $ toPolygon pg
      --             | pg <- fCollection^..geofeatures.traverse.geometry._Polygon
      --             ]
      -- writeIpeFile outFp . singlePageFromContent $ out
