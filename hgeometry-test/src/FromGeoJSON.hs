{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import           Algorithms.Geometry.LineSegmentIntersection
import           Control.Lens
import           Control.Monad
import           Data.Aeson                                  (Value, eitherDecodeStrict')
import qualified Data.ByteString                             as BS
import           Data.Either
import           Data.Ext
import           Data.Foldable
import           Data.Foldable                               as F
import           Data.Geometry.Point
import           Data.Geometry.Polygon                       as H
import           Data.Geospatial                             as Geo
import           Data.Hashable
import           Data.LinearRing
import           Data.List
import           Data.Sequence                               as Seq
import           Data.Serialize
import qualified Data.Set                                    as Set
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
        -- putStrLn $ file ++ ": " ++ show (hash inp)
        let mbGeo = eitherDecodeStrict' inp
        case mbGeo :: Either String (GeoFeatureCollection Value) of
          Left err  -> error $ "Invalid GeoJSON: " ++ file ++ "\n" ++ err
          Right geo -> pure $ map fromGeoPolygon $ getPolygons geo
      let (simple', multi') = partitionEithers $ nub $ concat polygons
          simple = Prelude.filter isValidPolygon simple'
          multi = Prelude.filter isValidPolygon multi'
      -- forM_ multi print
      print (sum $ map (F.length . view outerBoundaryVector) simple)
      print (F.length simple, F.length multi)
      BS.writeFile "polygons.simple" (encode $ map flattenPolygon simple)
      BS.writeFile "polygons.multi" (encode $ map flattenMultiPolygon multi)
      return ()
    _ -> printUsage

isValidPolygon :: Polygon t p Double -> Bool
isValidPolygon = not . hasSelfIntersections . fmap (realToFrac :: Double -> Rational)

flattenPolygon :: SimplePolygon p r -> [(r,r)]
flattenPolygon = map (unpack . view core) . F.toList . view outerBoundaryVector
  where
    unpack (Point2 a b) = (a,b)

flattenMultiPolygon :: MultiPolygon p r -> [[(r,r)]]
flattenMultiPolygon p =
  flattenPolygon (p^.outerBoundary) : map flattenPolygon (p ^. polygonHoles)

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

fromGeoPolygon :: GeoPolygon -> SomePolygon () Double
fromGeoPolygon p =
  case map fromRing $ F.toList (p^.unGeoPolygon) of
    []  -> error "Invalid polygon"
    [x] -> Left x
    (x:xs) ->
      Right $ H.MultiPolygon x xs
  where
    fromRing :: LinearRing GeoPositionWithoutCRS -> SimplePolygon () Double
    fromRing =
      fromPoints . map (ext . fromPointXY . retrieveXY) . nub . fromLinearRing

    fromPointXY :: PointXY -> Point 2 Double
    fromPointXY (PointXY x y) = realToFrac <$> Point2 x y
