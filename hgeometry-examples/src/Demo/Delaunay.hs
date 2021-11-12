{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
                  ! attr SLayer "delaunayTriangulation"
             , iO $ drawTree' emst ! attr SLayer "emst"
             ]
      outputFile = singlePageFromContent out
  outputFile' <- addStyleSheetFrom "../hgeometry-ipe/resources/opacities.isy" outputFile
  writeIpeFile outFile outputFile'

-- | The world in which the delaunay triangulation "lives"
data DTWorld

-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                         :: IpeOut' Maybe (VertexId' s, VertexData r (IpeAttributes IpeSymbol r)) IpeSymbol r
drawVtx (_vi, VertexData p ats) = Just $ defIO p ! ats

-- | Draw edges using normal line segments
drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

-- | Internal faces are filled polygons.
drawInternalFace                 :: PlanarSubdivision s v e f r
                                 -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    Path r
drawInternalFace s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan

-- | Draw the outer face (in some box)
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
drawOuterFace (_, pg :+ _) = Just $ defIO pg ! attr SOpacity "10%"
                                             ! attr SFill lightgray

  -- const Nothing
