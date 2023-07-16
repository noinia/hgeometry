{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module VoronoiDiagram.VoronoiSpec
  ( spec
  ) where

import Control.Lens
import Data.Default.Class
import Golden
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.VoronoiDiagram
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Voronoi diagram tests" $ do
  -- prop "voronoi vertex is center disk" $ \c ->
  --   voronoiVertices inputs
  it "trivial voronoi diagram" $
    voronoiVertices inputs `shouldBe` [Point2 5 5]
  runIO $ do out <- testIpe [osp|data/VoronoiDiagram/voronoi.ipe|]
             writeIpePage [osp|/tmp/voronoi.ipe|] (fromContent out)

  -- goldenWith [osp|data/golden/VoronoiDiagram/|]

  -- testCases [osp|Polygon/Simple/pointInPolygon.ipe|]
  -- numericalSpec

instance Default (Point 2 R) where
  def = error "not def"

inputs :: [Point 2 R]
inputs = [origin, Point2 10 10, Point2 10 0]


testIpe      :: OsPath -> IO [IpeObject R]
testIpe inFp = do (points :: [Point 2 R :+ _]) <- readAllFrom inFp
                  let vs = voronoiVertices $ (view core) <$> points
                  pure $ (map iO' points)
                         <>
                         [ iO'' v $ attr SStroke red
                         | v <- vs ]
