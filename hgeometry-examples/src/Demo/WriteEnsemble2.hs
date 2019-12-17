{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Control.Lens hiding ((<.>))
import Control.Monad
import Data.Ext
import Control.Applicative
import Data.Fixed
import qualified Data.Text as T
import Data.Maybe
import Data.Geometry.Ipe
import Data.Geometry
import Data.Geometry.PolyLine(fromPoints')
import System.Environment
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Foldable as F
import System.Directory
import Data.List(isSuffixOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import Data.Monoid
import qualified Data.HashMap.Strict as H
import Text.Printf
import qualified Data.List as L

import Network.HTTP.Conduit

--------------------------------------------------------------------------------0

-- http://www.spc.noaa.gov/exper/sref/srefplumes/returndata.php?search=OKC-09-3hrly-QPF&file=json_sid/20160309_09/OKC&mem=:&means=

data RunTime = R03 | R09 | R15 | R21 deriving (Eq,Show)

instance PrintfArg RunTime where
  formatArg v _ = \s -> (tail . show $ v) ++ s

locs :: [String]
locs = [ "OKC", "GRR", "BHM"
       , "LAX", "ICT", "JAX", "VQV","HMQ","AST","EVW"
       ]

fetch :: String -> Day -> RunTime -> String -> IO (Maybe (B.ByteString, Ensemble))
fetch loc day runTime setType = f <$> simpleHttp (url loc day runTime setType)
  where
    f :: B.ByteString -> Maybe (B.ByteString, Ensemble)
    f b = decode b >>= \(String t) -> let b' = C.pack . T.unpack $ t in (b',) <$> decode b'


fetchAll        :: Day -> RunTime -> IO [(FilePath,(B.ByteString, Ensemble))]
fetchAll day rt = catMaybes <$> (mapM (onSnd $ uncurry4 fetch) $ allCombos day rt)
  where
    onSnd f (a,b) = fmap (a,) <$> f b
    uncurry4 f (a,b,c,d) = f a b c d


allCombos        :: Day -> RunTime -> [(FilePath, (String, Day, RunTime, String))]
allCombos day rt = let (y,m,d) = toGregorian day
                   in  [ ( L.intercalate "-" [ loc, plot :: String, printf "%i%.2i%.2i_%v" y m d rt]
                         , (loc,day,rt,plot)
                         )
                       | loc <- locs, plot <- plots
                       ]

writeAll               :: FilePath -> Day -> RunTime -> IO ()
writeAll outDir day rt = do
                    xs <- fetchAll day rt
                    forM_ xs $ \(fp,(b,ens)) -> do
                      B.writeFile (outDir </> fp <.> "json") b
                      writeIpeFile (outDir </> fp <.> "ipe") . singlePageFromContent . toPolies $ ens



url :: String -> Day -> RunTime -> String -> String
url loc day runTime setType = let (y,m,d) = toGregorian day in
  printf "http://www.spc.noaa.gov/exper/sref/srefplumes/returndata.php?search=%s-%v-%s&file=json_sid/%i%.2i%.2i_%v/%s&mem=:&means=" loc runTime setType y m d runTime loc



plots = [ "3hrly-TMP"
        , "3hrly-DWP"
        , "3h-MUCAPE"
        , "3h-MLCAPE"
        , "3h-EFFSHR"
        , "3hrly-QPF"
        , "Total-QPF"
        , "3hrly-SNO"
        , "Total-SNO"
        , "3hr-2mRH%25"
        , "3h-10mWND"
        ]



toDay :: Integer -> UTCTime
toDay = posixSecondsToUTCTime . fromInteger . (`div` 1000)


data DataPoint = DataPoint { _time :: Integer
                           , _measuremnt :: Centi
                           } deriving (Show,Eq)


instance FromJSON DataPoint where
  parseJSON (Array v) = uncurry DataPoint <$> parse (F.toList v)
    where
      parse [iv, String vv] = (, read $ T.unpack vv) <$> parseJSON iv
      parse [iv, vv]        = (,) <$> parseJSON iv <*> parseJSON vv
      parse _               = fail "no parse"

data EnsembleItem = EnsembleItem { _data  ::  [DataPoint]
                                 , _color :: T.Text
                                 } deriving (Show,Eq)

instance FromJSON EnsembleItem where
  parseJSON (Object v) = EnsembleItem <$> v .: "data"
                                      <*> v .: "color"

toPolyLine :: [DataPoint] -> PolyLine 2 () Centi
toPolyLine ps@(s:_) = fromPoints' $
                      zipWith (\x (DataPoint i v) -> Point2 (15 * fromIntegral x) v) [0..] ps


toIpePoly    :: EnsembleItem -> IpeObject Centi
toIpePoly ei = let pl = toPolyLine . _data $ ei
                   c  = _color ei
               in asIpeObject pl mempty

newtype Ensemble = Ensemble { _unEns :: [(T.Text,EnsembleItem)] } deriving (Show,Eq)

toPolies = map (toIpePoly . snd) . _unEns



instance FromJSON Ensemble where
  parseJSON (Object v) = Ensemble <$> traverse (\k -> (k,) <$> v .: k) keys


keys = [ "ARWC"
       , "ARN1"
       , "ARN2"
       , "ARN3"
       , "ARN4"
       , "ARN5"
       , "ARN6"
       , "ARP1"
       , "ARP2"
       , "ARP3"
       , "ARP4"
       , "ARP5"
       , "ARP6"
       , "MBCN"
       , "MBN1"
       , "MBN2"
       , "MBN3"
       , "MBN4"
       , "MBN5"
       , "MBN6"
       , "MBP1"
       , "MBP2"
       , "MBP3"
       , "MBP4"
       , "MBP5"
       , "MBP6"]





-- {"ARWC":{"data": [[1457503200000, "52.66"], [1457514000000, "49.90"], [1457524800000, "47.31"], [1457535600000, "51.19"], [1457546400000, "55.25"], [1457557200000, "57.71"], [1457568000000, "59.32"], [1457578800000, "56.61"], [1457589600000, "55.51"], [1457600400000, "53.74"], [1457611200000, "52.90"], [1457622000000, "53.11"], [1457632800000, "59.12"], [1457643600000, "61.87"], [1457654400000, "60.40"], [1457665200000, "55.27"], [1457676000000, "52.99"], [1457686800000, "51.20"], [1457697600000, "49.98"], [1457708400000, "48.75"], [1457719200000, "46.41"], [1457730000000, "47.37"], [1457740800000, "49.69"], [1457751600000, "51.07"], [1457762400000, "51.90"], [1457773200000, "52.21"], [1457784000000, "51.94"], [1457794800000, "54.06"], [1457805600000, "59.26"]], "label": "ARWC", "color": "#cc3300"},


main = getArgs >>= mainWith


a </> b = mconcat [a, "/", b]
a <.> b = mconcat [a, ".", b]

mainWith (inPath:outPath:_) = do
    files <- filter (".json" `isSuffixOf`) <$> getDirectoryContents inPath
    mapM_ (\iF -> process (inPath </> iF) (outPath </> iF <.> "ipe")) files

process inPath outPath = do
    putStrLn $ "Processing " <> inPath
    Just ens <- decode <$> B.readFile inPath
    writeIpeFile outPath . singlePageFromContent . toPolies $ ens
