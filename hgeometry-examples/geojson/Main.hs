{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import           Data.Aeson
import           Data.Geospatial
import           HGeometry.GeoJSON
import           Ipe
-- import           Paths_hgeometry_examples
import qualified System.File.OsPath as File
import           System.OsPath
--------------------------------------------------------------------------------

-- type R = Double

parseGeoJSONFile :: OsPath -> IO (Either String (GeoFeatureCollection Value))
parseGeoJSONFile = fmap eitherDecode . File.readFile

main :: IO ()
main = do
  res <- parseGeoJSONFile [osp|data/ne_110m_admin_1_states_provinces_shp.geojson|]
  case res of
    Left err          -> print err
    Right fCollection -> do
      let pgs = fCollection^..geofeatures.folded.geometry._Polygon._GeoPolygonPolygonalDomain
      let outFp = [osp|foo.ipe|]
          out   = [ iO $ ipePolygon pg
                  | pg <- pgs
                  ]
      writeIpeFile outFp . singlePageFromContent $ out
