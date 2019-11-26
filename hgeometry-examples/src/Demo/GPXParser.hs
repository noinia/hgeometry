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
import           Data.Semigroup
import           Data.Time.Clock
import           Data.Time.Format
import           Text.XML.Expat.Tree
import qualified Data.Text as Text
import           Debug.Trace

--------------------------------------------------------------------------------

-- type Time = UTCTime
type Time = Text.Text


newtype Position = Position {_unP :: Point 2 Rational}  deriving (Show,Eq)
makeLenses ''Position

latitude :: Lens' Position Rational
latitude = unP.xCoord

longitude :: Lens' Position Rational
longitude = unP.yCoord


newtype TrackPoint = TP {_unTP :: Position :+ Time} deriving (Show,Eq)
makeLenses ''TrackPoint

newtype Track = Track { _trackPoints :: [TrackPoint] } deriving (Show,Eq)
makeLenses ''Track

newtype Activity = Activity { _tracks :: [Track]} deriving (Show,Eq)
makeLenses ''Activity


combineTracks (Activity ts) = Activity [Track $ concatMap _trackPoints ts]


readGPXFile    :: FilePath -> IO Activity
readGPXFile fp = (r . fst . parse defaultParseOptions) <$> B.readFile fp
  where
    -- l m = error . show $ m
    r = fromJust . parseGPX

class ReadGPX t where
  parseGPX :: Node (Text.Text) (Text.Text) -> Maybe t

instance ReadGPX Activity where
  parseGPX x = case selectPath ["gpx"] x of
    [x@(Element _ _ chs)] -> Just . Activity . mapMaybe parseGPX . chsWith "trk" $ x


    --                      concatMap (selectPath [""Track"]) $ chs
    -- _                 -> Nothing



instance ReadGPX Track where
  parseGPX x@(Element "trk" _ _) = Just . Track . mapMaybe parseGPX . concatMap (chsWith "trkpt") . chsWith "trkseg" $ x




instance ReadGPX TrackPoint where
  parseGPX x@(Element "trkpt" ats _) = (\p t -> TP $ p :+ t) <$> pos <*> time
    where
      pos  = (\l l' -> Position $ Point2 l l') <$> lat <*> lon
      time = fmap (readTime' . extract) . listToMaybe . chsWith "time" $ x

      read' = realToFrac . read @Double . Text.unpack

      lat = read' <$> lookup "lat" ats
      lon = read' <$> lookup "lon" ats


extract = (\(Text s) -> s) . head . eChildren

-- readTime' :: String -> UTCTime
-- readTime' = parseTimeOrError True defaultTimeLocale "%0Y-%m-%dT%TZ"

readTime' :: Text.Text -> Time
readTime' = id

-- instance ReadGPX Position where
--   parseGPX x@(Element "Position" _ _) = (\l l' -> Position $ Point2 l l') <$> lat <*> lon

--     where
--       f n = listToMaybe . map (read . extract) . (chsWith n) $ x
--       lat = f "LatitudeDegrees"
--       lon = f "LongitudeDegrees"

selectPath :: [Text.Text] -> Node Text.Text Text.Text -> [Node Text.Text Text.Text]
selectPath []  _ = []
selectPath [n] x | hasName n x = [x]
                 | otherwise   = []
selectPath (n:p) x | hasName n x = concatMap (selectPath p) $ eChildren x
                   | otherwise   = []

chsWith   :: Text.Text -> Node Text.Text Text.Text -> [Node Text.Text Text.Text]
chsWith n = filter (hasName n) . eChildren

hasName   :: Text.Text -> Node Text.Text Text.Text -> Bool
hasName _ (Text _) = False
hasName n (Element n' _ _) = n == n'
