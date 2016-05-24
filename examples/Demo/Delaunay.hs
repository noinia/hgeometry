{-# LANGUAGE ScopedTypeVariables #-}
module Demo.Delaunay where

import Control.Applicative
import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import Data.Data
import Data.Ext
import Data.Traversable
import Data.Fixed
import Data.Geometry
import Data.Geometry.Ipe
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import Options.Applicative

data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Compute the Delaunay Triangulation of the points in the input file."
               <> header   "Delaunay"
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
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> case page^..content.traverse._IpeUse of
        []         -> putStrLn "No points found"
        syms@(_:_) -> do
           let pts  = map (&extra .~ ()) $ map (\s -> s&core %~ (^.symbolPoint)) syms
               dt  = delaunayTriangulation $ NonEmpty.fromList pts
               out = [asIpe drawTriangulation dt]
           writeIpeFile outFile . singlePageFromContent $ out
