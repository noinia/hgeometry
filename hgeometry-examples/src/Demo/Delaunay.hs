{-# LANGUAGE ScopedTypeVariables #-}
module Demo.Delaunay where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Geometry.EuclideanMST
import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.PlanarSubdivision.Draw
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.Semigroup
import           Data.Tree.Draw
import           Ipe
import           Ipe.Color
import           Options.Applicative

--------------------------------------------------------------------------------

type R = RealNumber 5

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
  pts <- readAllFrom @(Point 2 R) inFile
  let pts' = NonEmpty.fromList pts
      dt   = toPlanarSubdivision (Proxy @DTWorld) . delaunayTriangulation $ pts'
      emst = euclideanMST pts'
      out  = [ iO $ drawPlanarSubdivisionWith drawVtx drawEdge (drawInternalFace dt) drawOuterFace dt
             , iO $ drawTree' emst
             ]
  writeIpeFile outFile . singlePageFromContent $ out

-- | The world in which the delaunay triangulation "lives"
data DTWorld

-- | Draw vertices using their default representation; filled marks.
drawVtx                       :: IpeOut' Maybe (VertexId' s, VertexData r v) IpeSymbol r
drawVtx (_vi, VertexData p v) = Just $ defIO p

-- |
drawEdge :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

drawInternalFace                 :: PlanarSubdivision s v e f r
                         -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    Path r
drawInternalFace s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan

drawOuterFace :: IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f)    fi r
drawOuterFace = const Nothing
