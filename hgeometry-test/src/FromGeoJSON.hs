module Main where

import Data.Geospatial
import Data.Aeson
import Data.Sequence as Seq
import System.Environment
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Geometry.Polygon ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      mbGeo <- decodeFileStrict file
      case mbGeo :: Maybe (GeoFeatureCollection Value) of
        Nothing -> error $ "Invalid GeoJSON"
        Just geo -> do
          forM_ (getPolygons geo) $ \p -> do
            let n = Seq.length (p^.unGeoPolygon)
            putStrLn $ "Polygon: " ++ show n
    _ -> return ()

getPolygons :: GeoFeatureCollection a -> [GeoPolygon]
getPolygons geo = concat
  [ case feature^.geometry of
      Polygon p -> [p]
      MultiPolygon mp -> toList $ splitGeoMultiPolygon mp
      _ -> []
  | feature <- toList $ geo^.geofeatures ]
