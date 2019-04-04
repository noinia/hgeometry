{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Demo.GPXParser where



import           Control.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Ext
import           Data.Geometry.Point
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Text.XML.Expat.Tree

--------------------------------------------------------------------------------

type Time = UTCTime

newtype Position = Position {_unP :: Point 2 Double}  deriving (Show,Eq)
makeLenses ''Position

latitude :: Lens' Position Double
latitude = unP.xCoord

longitude :: Lens' Position Double
longitude = unP.yCoord


newtype TrackPoint = TP {_unTP :: Position :+ Time} deriving (Show,Eq)
makeLenses ''TrackPoint

newtype Track = Track { _trackPoints :: [TrackPoint] } deriving (Show,Eq)
makeLenses ''Track

newtype Activity = Activity { _tracks :: [Track]} deriving (Show,Eq)
makeLenses ''Activity


combineTracks :: Activity -> Activity
combineTracks (Activity ts) = Activity [Track $ concatMap _trackPoints ts]


readGPXFile    :: FilePath -> IO Activity
readGPXFile fp = (r . fst . parse defaultParseOptions) <$> B.readFile fp
  where
    -- l m = error . show $ m
    r = fromJust . parseGPX

class ReadGPX t where
  parseGPX :: Node String String -> Maybe t

instance ReadGPX Activity where
  parseGPX x = case selectPath ["gpx"] x of
    [y@(Element _ _ _)] -> Just . Activity . mapMaybe parseGPX . chsWith "trk" $ y
    --                      concatMap (selectPath [""Track"]) $ chs
    _                 -> Nothing



instance ReadGPX Track where
  parseGPX x@(Element "trk" _ _) = Just . Track . mapMaybe parseGPX . concatMap (chsWith "trkpt") . chsWith "trkseg" $ x
  parseGPX _ = error "ReadGPX.parseGPX for Track: Pattern match(es) are non-exhaustive"


instance ReadGPX TrackPoint where
  parseGPX x@(Element "trkpt" ats _) = (\p t -> TP $ p :+ t) <$> pos <*> time
    where
      pos  = (\l l' -> Position $ point2 l l') <$> lat <*> lon
      time = fmap (readTime' . extract) . listToMaybe . chsWith "time" $ x

      lat = read <$> lookup "lat" ats
      lon = read <$> lookup "lon" ats
  parseGPX _ = error "ReadGPX.parseGPX for TrackPoint: Pattern match(es) are non-exhaustive"


extract :: NodeG [] tag c -> c
extract = (\(Text s) -> s) . head . eChildren

readTime' :: String -> UTCTime
readTime' = parseTimeOrError True defaultTimeLocale "%0C%y-%m-%dT%TZ"

-- instance ReadGPX Position where
--   parseGPX x@(Element "Position" _ _) = (\l l' -> Position $ point2 l l') <$> lat <*> lon

--     where
--       f n = listToMaybe . map (read . extract) . (chsWith n) $ x
--       lat = f "LatitudeDegrees"
--       lon = f "LongitudeDegrees"

selectPath :: [String] -> Node String String -> [Node String String]
selectPath []  _ = []
selectPath [n] x | hasName n x = [x]
                 | otherwise   = []
selectPath (n:p) x | hasName n x = concatMap (selectPath p) $ eChildren x
                   | otherwise   = []

chsWith   :: String -> Node String String -> [Node String String]
chsWith n = filter (hasName n) . eChildren

hasName   :: String -> Node String String -> Bool
hasName _ (Text _) = False
hasName n (Element n' _ _) = n == n'
