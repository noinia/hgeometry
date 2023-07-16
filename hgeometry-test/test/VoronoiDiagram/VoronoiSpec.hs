module VoronoiDiagram.VoronoiSpec
  ( spec
  ) where

import Data.Default.Class
import Golden
import HGeometry.Ball
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.VoronoiDiagram
import Ipe
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
  -- testCases [osp|Polygon/Simple/pointInPolygon.ipe|]
  -- numericalSpec

instance Default (Point 2 R) where
  def = error "not def"

inputs :: [Point 2 R]
inputs = [origin, Point2 10 10, Point2 0 10]
