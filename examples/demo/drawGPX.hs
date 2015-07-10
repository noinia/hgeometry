{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module DrawGPX where

import           Control.Applicative
import           Control.Lens
import           Data.List(isSuffixOf)
import qualified Data.Sequence as S
import           Data.Time.Clock
import           GPXParser
import           System.Directory

import           Data.Geometry.PolyLine
import           Data.Geometry.Point
import           Data.Geometry.Ipe.Writer

import           Data.Ext



main = do
    let path = "/Volumes/Transcend/gpxf/"
    files <- map (path ++ ) . filter (isSuffixOf ".gpx") <$> getDirectoryContents path
    tks   <- concatMap (_tracks . combine)
             <$> mapM (\fp -> print fp >> readGPXFile fp) files
    let polies   = map (asPolyLine . subsampleTrack ssFactor) tks
        !polies' = drop 1 $ drop 1 $ take 3 $ take 7 $ drop 15 $ drop 120 $ map (unPolyLine.traverse.extra .~ ()) polies
    writePolyLineFile "/tmp/out.ipe" $ zipWith stroke polies' (cycle colors)


asPolyLine :: Track -> PolyLine 2 UTCTime Double
asPolyLine = PolyLine . S.fromList . map toPt . _trackPoints

toPt :: TrackPoint -> Point 2 Double :+ Time
toPt (TP (pos :+ t)) = mercatoProject world pos :+ t


ssFactor = 5

worldWidth  = 1000
worldHeight = 1000
world = (worldWidth,worldHeight)

colors = [ "red" , "purple" , "blue" , "green" , "orange"  ]

stroke p c = (p,[("stroke",c)])


groupsOf      :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf k xs = let (ys,xss) = splitAt k xs in ys : groupsOf k xss

subsample   :: Int -> [a] -> [a]
subsample k = map head . groupsOf k

subsampleTrack   :: Int -> Track -> Track
subsampleTrack k = over trackPoints (subsample k)


-- | Given the width and height of the map and a Position, compute a Mercato Projection of
-- the position. See
-- <http://en.wikipedia.org/wiki/Mercator_projection#Derivation_of_the_Mercator_projection
-- WikiPedia> for more info.
mercatoProject                    :: (Double,Double)
                                  -> Position
                                  -> Point 2 Double
mercatoProject (width,height) pos = point2 x y
  where
    x    =                (width / 360)    * pos^.longitude
    y    = (height / 2) - (width / (2*pi)) * (log . tan $ (pi / 4) + (latR / 2))
    latR = -1 * pos^.latitude * pi / 180
