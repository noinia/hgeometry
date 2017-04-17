{-# LANGUAGE ScopedTypeVariables #-}
module Demo.TriangulateWorld where

import Data.Semigroup
-- import Control.Monad.Except
import Data.Geometry.Ipe
import           Options.Applicative
import           Data.Semigroup
import           Control.Lens
import Data.Geometry.PlanarSubdivision
import           Data.Data
import           Data.Ext
import Algorithms.Geometry.PolygonTriangulation.MakeMonotone(makeMonotone)


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
          subdivs = map (\(pg :+ _) -> makeMonotone (Identity PX) pg) polies
          segs    = map (^._2.core) . concatMap edgeSegments $ subdivs
          out     = [ asIpeObject pg a
                    | pg :+ a <- polies
                    ] <>
                    [ asIpeObject s mempty
                    | s <- segs
                    ]
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

flattenGroups :: [IpeObject r] -> [IpeObject r]
flattenGroups = concatMap flattenGroups'

flattenGroups'                              :: IpeObject r -> [IpeObject r]
flattenGroups' (IpeGroup (Group gs :+ ats)) =
      map (applyAts ats) . concatMap flattenGroups' $ gs
    where
      applyAts ats = id
flattenGroups' o                            = [o]
