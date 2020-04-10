module Demo.WriteEnsemble where

import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Fixed
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.PolyLine (fromPoints)
import qualified Data.List as List
import           Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar
import           Options.Applicative
import           System.Directory


--------------------------------------------------------------------------------

data Options = Options { kind    :: String
                       , inPath  :: FilePath
                       , outPath :: FilePath
                       }
               deriving Data


options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Converts ensembles to ipe files"
               <> header   "ensemblewriter - writes a weather ensembles to a ipe files"
               )
  where
    parser = Options
          <$> strOption (help "Kind of input data in the input files" )
          <*> strOption (help "Input Directory")
          <*> strOption (help "Output Directory")

--------------------------------------------------------------------------------

-- read a bunch of text files, each defining a time-series (ensemble), produce
-- an ipe file where each time-series is represented by a polyline.

main :: IO ()
main = execParser options >>= mainWith

mainWith                               :: Options -> IO ()
mainWith (Options kind inPath outPath) = do
    inFiles <- filter (".dat" `isSuffixOf`) <$> getDirectoryContents inPath
    let f = case kind of
          "precip" -> asPrecipPt
          _        -> asTempPt
    polies <- mapM (fmap (asPts f) . readFile' . ((inPath ++ "/") ++)) inFiles
    let polies' = map (fromPointsUnsafe . take 100) . trim $ polies
    writeIpeFile outPath . singlePageFromContent . map iO' $ polies'

readFile'    :: String -> IO T.Text
readFile' fp = putStrLn fp >> TIO.readFile fp

maxStartDay :: [[core :+ Day]] -> Day
maxStartDay = maximum . map ((^.extra) . List.head)

-- | Find the last starting day in the file, and trim all the lists s.t. they
-- all start at or after this day.
trim    :: [[Point 2 Milli :+ Day]] -> [[Point 2 Milli :+ Day]]
trim xs = let m      = maxStartDay xs
              startD = fromIntegral $ toModifiedJulianDay m
          in map ( map (\p -> p&core.xCoord %~ subtract startD)
                 . dropWhile (\x -> x^.extra < m)
                 ) xs


-- force'   :: Show r => IO (PolyLine 2 () r) -> IO (PolyLine 2 () r)
-- force' mkP = mkP >>= \p -> (putStrLn $ show p) >> return p

read' :: Read a => T.Text -> a
read' = read . T.unpack

asPts   :: ([T.Text] -> b) -> T.Text -> [b]
asPts f = map (f . T.words) . filter (\l -> T.head l /= '#') . T.lines

-- | read a line of the form: yyyy mm dd value
asTempPt    :: [T.Text] -> Point 2 Milli :+ Day
asTempPt ts = let [y,m,d] = map read' $ Prelude.init ts
                  v       = read' $ last ts
                  day     = fromGregorian y (fromInteger m) (fromInteger d)
              in Point2 (fromIntegral $ (toModifiedJulianDay day)) (10 * v) :+ day

-- | read a line of the form: yyyymmdd value
asPrecipPt       :: [T.Text] -> Point 2 Milli :+ Day
asPrecipPt [t,v] = let (y,t') = T.splitAt 4 t
                       (m,d)  = T.splitAt 2 t'
                       day    = fromGregorian (read' y) (read' m) (read' d)
                   in Point2 (fromIntegral $ (toModifiedJulianDay day)) (10 * read' v) :+ day
