{-# LANGUAGE ScopedTypeVariables #-}
module Demo.GeneratePlanarSubdivision where


import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlaneGraph
import           Data.PlanarGraph as PG
import           Data.Semigroup
import           Data.Yaml.Util
import           Options.Applicative
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances ()

data Options = Options { _outFile   :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Generate a random planar subdivision"
               <> header   "GenPlanar"
               )
  where
    parser = Options
          <$> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

data DT = DT

mainWith                   :: Options -> IO ()
mainWith (Options outFile) = do
    pts :: NonEmpty.NonEmpty (Point 2 Rational :+ ()) <- generate arbitrary
    let dt   = delaunayTriangulation . NonEmpty.fromList . NonEmpty.take 5 $ pts
        pg   = toPlaneGraph (Identity DT) dt
        out  = [iO $ drawTriangulation dt]
    print pg
    printYaml $ pg^.graph
    writeIpeFile outFile . singlePageFromContent $ out


readInput fp = decodeYamlFile fp >>= \case
                 Left e -> print e
                 Right (ps :: PlanarGraph DT Primal (VertexData Rational ()) () ()) -> do
                   mapM_ print $ PG.toAdjacencyLists ps
                   print ps


readPG inFile = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> runPage page
  where
    runPage page = do
      let polies  = page^..content.to flattenGroups.traverse._withAttrs _IpePath _asLineSegment
      mapM_ print polies
