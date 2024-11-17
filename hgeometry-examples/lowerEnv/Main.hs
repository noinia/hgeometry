{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.PlaneGraph
import           HGeometry.PlaneGraph.Instances
import           HGeometry.Point
import           HGeometry.Triangle
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope
import           Ipe
import           PLY.Writer
import           System.OsPath
import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5


myPlanes = NonEmpty.fromList $ zipWith (\i p -> pointToPlane p :+ (i,p)) [0..]
           [ Point2 16 80
           , Point2 64 48
           , Point2 208 128
           , Point2 176 48
           , Point2 96 112
           , Point2 128 80
           , Point2 48 144
           ]

verticesOf = NonEmpty.fromList . foldMap F.toList . trianglesOf
trianglesOf _ = [ Triangle (origin :+ 0) (Point3 10 0 1  :+ 1) (Point3 0 10 2  :+ 2) ]


  -- \case
  -- ParallelStrips _      -> undefined
  -- ConnectedEnvelope env -> undefined

-- trianglesOf env = []

main :: IO ()
main = renderOutputToFile [osp|myLowerEnv.ply|] (verticesOf $ lowerEnvelope myPlanes)
                                                (trianglesOf $ lowerEnvelope myPlanes)
