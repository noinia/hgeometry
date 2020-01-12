{-# LANGUAGE ScopedTypeVariables #-}
module Demo.TriangulateWorld where

import           Algorithms.Geometry.LineSegmentIntersection (hasSelfIntersections)
import           Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann (interiorIntersections)
import           Algorithms.Geometry.PolygonTriangulation.MakeMonotone (makeMonotone)
import           Algorithms.Geometry.PolygonTriangulation.Triangulate (triangulate)
import           Control.Lens
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ipe
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Polygon
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Semigroup
import           Options.Applicative

--------------------------------------------------------------------------------

data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Triangulate all polygons in the input file."
               <> header   "trianguldateWorld"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

data PX = PX

mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> runPage page
  where
    runPage page = do
      let polies  = page^..content.to flattenGroups.traverse._withAttrs _IpePath _asSimplePolygon
          polies' = filter (not . hasSelfIntersections . (^.core)) polies
          intersections' = concatMap (Map.keys . interiorIntersections
                                      . listEdges . (^.core)) polies
          subdivs = map (\(pg :+ _) -> triangulate (Identity PX) pg) polies'
          yMonotones = mapMaybe (^?_2.core._Left)
                     . concatMap (F.toList.rawFacePolygons) $ subdivs
          ofs = map (\s -> rawFaceBoundary (outerFaceId s) s) subdivs
          segs    = map (^._2.core) . concatMap (F.toList . edgeSegments) $ subdivs
          out     = mconcat [ [ iO' pg | pg <- polies ]
                            , [ iO' s  | s  <- segs ]
                            , [ iO' pg | pg <- yMonotones ]
                            ]
      putStrLn $ "#polygons found: " <> show (length polies)

      putStrLn $ "first <=100 self-intersections: "
      mapM_ print $ take 100 intersections'
      putStrLn $ "number of non-self intersecting polygons: " <> show (length polies')

      mapM_ print . map (\pg -> pg^.core.to polygonVertices.to length) $ polies'
      writeIpeFile outFile . singlePageFromContent $ out


-- mainWith                          :: Options -> IO ()
-- mainWith (Options inFile outFile) = do
--     ePage <- readSinglePageFile inFile
--     case ePage of
--       Left err                         -> print err
--       Right (page :: IpePage Rational) -> runPage page
--   where
--     runPage page = do
--       let orig = page^.content
--           all' = page^.content.to flattenGroups
--       writeIpeFile outFile . singlePageFromContent $ orig <> all'


-- type ValT = EitherT IO

-- flattenGroups :: [IpeObject r] -> [IpeObject r]
-- flattenGroups = concatMap flattenGroups'

-- flattenGroups'                              :: IpeObject r -> [IpeObject r]
-- flattenGroups' (IpeGroup (Group gs :+ ats)) =
--       map (applyAts ats) . concatMap flattenGroups' $ gs
--     where
--       applyAts ats = id
-- flattenGroups' o                            = [o]

-- runExcept'   :: (Show e) => ExceptT e IO () -> IO ()
-- runExcept' m = runExceptT m >>= \case
--                 Left e   -> print e
--                 Right () -> pure ()

-- mainWith                          :: Options -> IO ()
-- mainWith (Options inFile outFile) = runExcept' $ do
--     (page :: IpePage Rational) <- readSinglePageFile inFile
--     let polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
--     let out = undefinedL
--     lift $  writeIpeFile outFile . singlePageFromContent $ out
