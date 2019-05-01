{-# LANGUAGE ScopedTypeVariables #-}
module Demo.Delaunay where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Geometry.EuclideanMST.EuclideanMST
import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Geometry
import           Data.Tree.Draw
import           Data.Geometry.Triangulation.Draw
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import           Options.Applicative


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
           let pts  = syms&traverse.core %~ (^.symbolPoint)
               pts' = NonEmpty.fromList pts
               dt   = delaunayTriangulation $ pts'
               emst = euclideanMST pts'
               out  = [iO $ drawTriangulation dt, iO $ drawTree' emst]
           -- print $ length $ edges' dt
           -- print $ toPlaneGraph (Proxy :: Proxy DT) dt
           writeIpeFile outFile . singlePageFromContent $ out


data DT

--xs = [(1,(1,1)) , (2,(2,2)) ]
