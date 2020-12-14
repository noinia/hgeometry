{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString       as BS
import           Data.Either
import           Data.Ext
import           Data.Foldable
import           Data.Foldable         as F
import           Data.Geometry.Point
import           Data.Geometry.Polygon as H
import           Data.Geospatial       as Geo
import           Data.Hashable
import           Data.LinearRing
import           Data.List
import           Data.Sequence         as Seq
import qualified Data.Set              as Set
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("geojson":files) -> do
      polygons <- forM files $ \file -> do
        inp <- BS.readFile file
        putStrLn $ file ++ ": " ++ show (hash inp)
        let mbGeo = eitherDecodeStrict' inp
        case mbGeo :: Either String (GeoFeatureCollection Value) of
          Left err  -> error $ "Invalid GeoJSON: " ++ file ++ "\n" ++ err
          Right geo -> pure $ map fromGeoPolygon $ getPolygons geo
      let (simple, multi) = partitionEithers $ nub $ concat polygons
      -- forM_ multi print
      print (F.length simple, F.length multi)
      return ()
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  hPutStrLn stderr $ "Usage: " ++ prog ++ " geojson [FILE]"
  hPutStrLn stderr $ "Generates out.simple and out.multi polygon files."
  exitSuccess

getPolygons :: GeoFeatureCollection a -> [GeoPolygon]
getPolygons geo = concat
  [ case feature^.geometry of
      Geo.Polygon p       -> [p]
      Geo.MultiPolygon mp -> toList $ splitGeoMultiPolygon mp
      _                   -> []
  | feature <- toList $ geo^.geofeatures ]

fromGeoPolygon :: GeoPolygon -> SomePolygon () Rational
fromGeoPolygon p =
  case map fromRing $ F.toList (p^.unGeoPolygon) of
    []  -> error "Invalid polygon"
    [x] -> Left x
    (SimplePolygon pts:xs) ->
      Right $ H.MultiPolygon pts xs
  where
    fromRing :: LinearRing GeoPositionWithoutCRS -> SimplePolygon () Rational
    fromRing = fromPoints . map (ext . fromPointXY . retrieveXY) . fromLinearRing

    fromPointXY :: PointXY -> Point 2 Rational
    fromPointXY (PointXY x y) = realToFrac <$> Point2 x y
