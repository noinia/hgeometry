{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import           Control.Monad (replicateM)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.Sample
import           HGeometry.Transformation
import           HGeometry.Triangle
import           Ipe
import qualified System.File.OsPath as File
import           System.OsPath
import           System.Random.Stateful
--------------------------------------------------------------------------------

type R = Double

targetPolygon :: SimplePolygon (Point 2 R)
targetPolygon = scaleUniformlyBy 20 $ fromJust $ fromPoints
  [ Point2 0 0
  , Point2 1 0
  , Point2 1 1, Point2 2 1, Point2 2 (-1)
  , Point2 0 (-1), Point2 0 (-2)
  , Point2 3 (-2), Point2 3 2, Point2 0 2
  ]

sampler :: Sampler R (Triangle (Point 2 R))
sampler = triangleSampler $ targetPolygon :| []

numPoints :: Int
numPoints = 1000

samples   :: StatefulGen g IO => g -> IO [Point 2 Double]
samples g = replicateM numPoints (samplePoint sampler g)

main :: IO ()
main = do
  pts <- samples globalStdGen
  let outFp = [osp|foo.ipe|]
      out   = [ iO $ defIO targetPolygon ]
               <>
              [ iO $ defIO p
              | p <- pts
              ]
  writeIpeFile outFp . singlePageFromContent $ out
