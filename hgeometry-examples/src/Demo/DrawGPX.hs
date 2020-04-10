{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Demo.DrawGPX where

import           Algorithms.Geometry.PolyLineSimplification.DouglasPeucker
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.PolyLine
import           Data.Geometry.Vector
import           Data.List (isSuffixOf)
import qualified Data.List as List
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Demo.GPXParser
import           Options.Applicative
import           System.Directory
import           Text.Printf (printf)


--------------------------------------------------------------------------------

data Options = Options { _inPath  :: FilePath
                       , _outPath :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Draws gpx trajectories in ipe"
               <> header   "DrawGPX"
               )
  where
    parser = Options
          <$> strOption (help "Input Directory"
                         <> short 'i'
                         <> metavar "INDIR"
                        )
          <*> strOption (help "Output File"
                         <> short 'o'
                         <> metavar "TARGET"
                        )

--------------------------------------------------------------------------------

mainWith                          :: Options -> IO ()
mainWith (Options inPath outPath) = do
    let inPath' = inPath ++ "/"
    files <- map (inPath' ++) . filter (isSuffixOf ".gpx")
         <$> getDirectoryContents inPath'
    tks   <- concatMap (_tracks . combineTracks) <$> mapM readGPXFile files
    let polies  = mapMaybe asPolyLine tks
        polies' = map (douglasPeucker 0.01 . scaleUniformlyBy 100) polies
        pg = singlePageFromContent $ map (iO . defIO) polies'
    -- print pg
    writeIpeFile outPath pg


colors :: [T.Text]
colors = map (T.unwords . map (T.pack . printf "%.4f" . (/ 256.0))) colors'
  where
    colors' :: [[Double]]
    -- colors' = [ [84,48,5]
    --           , [140,81,10]
    --           , [191,129,45]
    --           , [223,194,125]
    --           , [246,232,195]
    --           , [245,245,245]
    --           , [199,234,229]
    --           , [128,205,193]
    --           , [53,151,143]
    --           , [1,102,94]
    --           , [0,60,48]
    --           , [0,0,0]
    --           ]
    colors' = [ [166,206,227]
              , [31,120,180]
              , [178,223,138]
              , [51,160,44]
              , [251,154,153]
              , [227,26,28]
              , [253,191,111]
              , [255,127,0]
              , [202,178,214]
              , [106,61,154]
              , [255,255,153]
              , [177,89,40]
              ]

-- readCoords    :: FilePath -> IO (PolyLine 2 () Double)
-- readCoords fp = fromPoints .
--                 map ((\[x,y] -> Point2 x y :+ ()) . map read . words) . lines
--              <$> readFile fp

-- readCoords'    :: FilePath -> IO [PolyLine 2 () Double]
-- readCoords' fp = mapMaybe (fmap fromPoints . g . f)  .  group' . lines <$> readFile fp
--   where
--     f = map ((\[x,y] -> Point2 x y :+ ()) . map read . words)
--     g xs@(_:_:_) = Just xs
--     g _          = Nothing

-- group' lst = case break (== "NL") lst of
--                ([],[]) -> []
--                ([],"NL":r) -> group' r
--                (pr,"NL":r) -> pr:group' r
--                (pr,[])     -> [pr]



-- maps = mapM (\f -> readCoords $ "/Users/frank/tmp/bikerides/maps/" ++ f)
--        [ "nld_coords.txt"
--        , "bel_coords.txt"
--        , "dnk_coords.txt"
--        , "fra_coords.txt"
--        ]

-- --   do
-- --     nld <- readCoords
-- --     writePolyLineFile "/tmp/nld.ipe" $  map (flip stroke "black") [nld]






asPolyLine :: Track -> Maybe (PolyLine 2 Time Rational)
asPolyLine = fromPoints <=< f . map toPt . _trackPoints
  where
    f xs@(_:_:_) = Just xs
    f _          = Nothing

toPt :: TrackPoint -> Point 2 Rational :+ Time
toPt (TP (pos :+ t)) = Point2 (pos^.longitude) (pos^.latitude) :+ t

ssFactor = 1

worldWidth  = 1000
worldHeight = 1000
world = (worldWidth,worldHeight)

-- colors = [ "red" , "purple" , "blue" , "green" , "orange"  ]

strokeByMonth p = stroke p c
  where
    dt = p^.points.singular (ix 0).extra
    (_,m,_) = toGregorian $ utctDay dt
    c = colors !! (m -1)

stroke p c = (p&points.traverse.extra .~ (),[("stroke",c)])


groupsOf      :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf k xs = let (ys,xss) = splitAt k xs in ys : groupsOf k xss

subsample   :: Int -> [a] -> [a]
subsample k = map List.head . groupsOf k

subsampleTrack   :: Int -> Track -> Track
subsampleTrack k = over trackPoints (subsample k)


-- | Given the width and height of the map and a Position, compute a Mercato Projection of
-- the position. See
-- <http://en.wikipedia.org/wiki/Mercator_projection#Derivation_of_the_Mercator_projection
-- WikiPedia> for more info.
mercatoProject                    :: (Double,Double)
                                  -> Position
                                  -> Point 2 Double
mercatoProject (width,height) pos = Point2 x y
  where
    lon = realToFrac $ pos^.longitude
    lat = realToFrac $ pos^.latitude

    x    =                (width / 360)    * lon
    y    = (height / 2) - (width / (2*pi)) * (log . tan $ (pi / 4) + (latR / 2))
    latR = -1 * lat * pi / 180
