{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Demo.DrawPlanarSubdivision where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Geometry.EuclideanMST
import           Control.Lens
import           Data.Data
import           Data.Ext
import           Geometry
import           Geometry.PlanarSubdivision
import           Geometry.PlanarSubdivision.Raw
import           Geometry.PlanarSubdivision.Draw
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph.Draw
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.Semigroup
import           Data.Tree.Draw
import           Ipe
import           Ipe.Color
import           Options.Applicative

import Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5

data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Draws a PlanarSubdivision in \"debugging style\""
               <> header   "DrawPSD"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

-- mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
  pts <- readAllFrom @(Point 2 R) inFile
  let pts' = NonEmpty.fromList pts
      dt   = toPlanarSubdivision @DTWorld . traceShowId . delaunayTriangulation $ pts'
      -- dt'   = toPlaneGraph @DTWorld . delaunayTriangulation $ pts'

      out  = [ iO $ drawPlanarSubdivisionWith drawVtx drawEdge (drawInternalFace dt) drawOuterFace dt
               -- iO $ drawPlaneGraphWith drawVtx drawEdge (drawInternalFace' dt') drawOuterFace' dt'
                  ! attr SLayer "delaunayTriangulation"
             ]
      outputFile = singlePageFromContent out
  outputFile' <- addStyleSheetFrom "../hgeometry-ipe/resources/opacities.isy" outputFile
  writeIpeFile outFile outputFile'

-- | The world in which the PSD
data DTWorld

--------------------------------------------------------------------------------

-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                       :: IpeOut' Maybe ( VertexId' s
                                               , VertexData r (IpeAttributes IpeSymbol r)
                                               ) Group r
drawVtx (vi, VertexData p ats) = Just $ labelledWith textAts id draw (p :+ show vi)
  where
    textAts :: IpeAttributes TextLabel r
    textAts = attr SStroke red
    draw p' = defIO p' ! ats


-- | Draw edges using normal line segments
drawEdge              :: Fractional r
                      => IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Group r
drawEdge (d, s :+ _) = Just $ labelledWith textAts midPoint draw' (s :+ d)
  where
    textAts  = mempty
    midPoint = interpolate (1/2)
    draw' s' = defIO s' ! attr SArrow normalArrow

-- | Internal faces are filled polygons.
drawInternalFace                 :: PlanarSubdivision s v e f r
                                 -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    Group r
drawInternalFace s (fi, pg :+ _) = Just $ ipeGroup [ iO $ defIO pg ! attr SFill lightcyan
                                                                   ! attr SOpacity "30%"

                                                   ]


-- | Draw the outer face (in some box)
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Group r
drawOuterFace (_, pg :+ _) = Just $ ipeGroup [ iO $   defIO pg ! attr SOpacity "30%"
                                                               ! attr SFill lightgray
                                             ]





--------------------------------------------------------------------------------
-- drawing plane graphs


  -- const Nothing
drawOuterFace' :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
drawOuterFace' (_, pg :+ _) = Just $ defIO pg ! attr SOpacity "30%"
                                              ! attr SFill lightgray

drawInternalFace'                 :: PlaneGraph s v e f r
                                 -> IpeOut' Maybe (FaceId' s,   SimplePolygon v r :+ f)    Path r
drawInternalFace' s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan
                                                    ! attr SOpacity "30%"



-- TODO: the Delaunay triangulation does not like 3 colinear points.
-- FIXME: The plane graph somehow misses one internal face
