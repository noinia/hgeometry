{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Demo.ExtractGPX where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon
import           Data.Geometry.Vector
import           Data.List (isSuffixOf)
import qualified Data.List as List
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Demo.GPXParser
import           Options.Applicative
import           System.Directory
import           Text.Printf (printf)


--------------------------------------------------------------------------------

data Options = Options { _inPath              :: FilePath
                       , _excludedRegionsPath :: FilePath
                       , _outPath             :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Converts gpx trajectories to a csv file"
               <> header   "ExtractGPX"
               )
  where
    parser = Options
          <$> strOption (help "Input Directory"
                         <> short 'i'
                         <> metavar "INDIR"
                        )
          <*> strOption (help "Excluded regions File"
                         <> short 'e'
                         <> metavar "EXCLUDEDREGIONS"
                        )
          <*> strOption (help "Output File"
                         <> short 'o'
                         <> metavar "TARGET"
                        )

--------------------------------------------------------------------------------

readTracks :: [FilePath] -> IO [(Track,FilePath)]
readTracks = fmap concat . mapM f
  where
    f fp = (map (,fp) . _tracks . combineTracks) <$> readGPXFile fp


processActivityFile                 :: FilePath -> [SimplePolygon () R] -> FilePath -> IO ()
processActivityFile outPath regs fp = do putStrLn $ "Processing " <> fp
                                         tks <- readTracks [fp]
                                         let tks' = clipTracks tks
                                         mapM_ (writeTrack outPath) tks'
    where
      clipTracks = concatMap (\(tk,fp) ->
                     zipWith (\i tk' -> (tk',fp,i)) [1..] $ clipExcludedRegions regs tk)

mainWith                          :: Options -> IO ()
mainWith (Options inPath exPath outPath) = do
    putStrLn "Reading Excluded Regions"
    regs <- readExcludedRegions <$> T.readFile exPath

    let inPath' = inPath ++ "/"
    files <- map (inPath' ++) . filter (isSuffixOf ".gpx")
         <$> getDirectoryContents inPath'

    mapM_ (processActivityFile outPath regs) files


    -- let polies  = mapMaybe asPolyLine tks
    --     polies' = map (douglasPeucker 0.01 . scaleUniformlyBy 100) polies
    --     pg = singlePageFromContent $ map (iO . defIO) polies'
    -- -- print pg
    -- writeIpeFile outPath pg
    -- writeFile


type R = Rational

readExcludedRegions :: T.Text -> [SimplePolygon () R]
readExcludedRegions = map readExcludedRegion . T.splitOn "\n\n"

readExcludedRegion :: T.Text -> SimplePolygon () R
readExcludedRegion = fromPoints . map readPt . T.lines
  where
    readPt   :: T.Text -> Point 2 R :+ ()
    readPt t = case (realToFrac . read @Double . T.unpack) <$> T.splitOn "," t of
                 [x,y] -> ext $ Point2 x y
                 _     -> error $ "failed to read " <> show t

clipExcludedRegions      :: [SimplePolygon () R] -> Track -> [Track]
clipExcludedRegions regs = map (Track . map fst)
                         . filter (not . snd . List.head)
                         . List.groupBy ((==) `on` snd)
                         . map (\tp@(TP (Position p :+ _)) ->
                                   (tp, any (p `insidePolygon`) regs))
                         . view trackPoints


writeTrack               :: FilePath -> (Track,FilePath, Int) -> IO ()
writeTrack dir (tk,fp,i) = T.writeFile (dir </> tn <.> "csv")
                         $ writeOutput (T.pack tn) tk
  where
    fn = T.unpack . List.last . T.splitOn "/" . T.pack $ fp
    tn = fn <.> show i

    a </> b = a <> "/" <> b
    a <.> b = a <> "." <> b


class Output t where
  writeOutput :: T.Text -> t -> T.Text

instance Output Track where
  writeOutput prefix = T.unlines . (header:) . map (writeOutput prefix) . view trackPoints
    where
      header = "PROBE_ID,LAT,LON,SAMPLE_DATE"

instance Output TrackPoint where
  writeOutput prefix (TP (p :+ t)) = T.intercalate "," [ prefix
                                                       , writeOutput "" $ p^.latitude
                                                       , writeOutput "" $ p^.longitude
                                                       , writeOutput "" t
                                                       ]
instance Output Double where
  writeOutput _ = T.pack . show

instance Output Rational where
  writeOutput _ = writeOutput @Double "" . realToFrac

instance Output Time where
  writeOutput _ = id

testT :: T.Text
testT = "52.114274, 5.305159"

test = T.splitOn "," testT


main = mainWith $ Options "/Users/frank/research/mtb_trajectory_data/in1"
                          "/Users/frank/research/mtb_trajectory_data/excluded_regions.csv"
                          "/Users/frank/research/mtb_trajectory_data/trajectories"
