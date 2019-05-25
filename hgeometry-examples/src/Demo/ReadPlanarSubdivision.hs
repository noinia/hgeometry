{-# LANGUAGE ScopedTypeVariables #-}
module Demo.ReadPlanarSubdivision where

import Control.Lens
import Data.Data
import Data.Ext
import Data.Geometry.Ipe
import Data.Geometry.Polygon
import Data.Geometry.PlanarSubdivision
import Data.Semigroup
import Options.Applicative
import qualified Data.Foldable as F




data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Reads an ipe file with polygons and builds a planar subdivision."
               <> header   "ReadPlanarsubdivision"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )


mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
  do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> runPage page
  where
    runPage page = do
      let polies  = page^..content.to flattenGroups.traverse._withAttrs _IpePath _asSimplePolygon
      mapM_ print polies



flattenGroups :: [IpeObject r] -> [IpeObject r]
flattenGroups = concatMap flattenGroups'

flattenGroups'                              :: IpeObject r -> [IpeObject r]
flattenGroups' (IpeGroup (Group gs :+ ats)) =
      map (applyAts ats) . concatMap flattenGroups' $ gs
    where
      applyAts ats = id
flattenGroups' o                            = [o]
