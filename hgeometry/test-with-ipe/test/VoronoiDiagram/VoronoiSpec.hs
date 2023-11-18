{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module VoronoiDiagram.VoronoiSpec
  ( spec
  ) where

import Control.Lens
import Data.Default.Class
import Golden
-- import HGeometry.Combinatorial.Util
-- import HGeometry.Duality
import HGeometry.Ext
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.NonVertical
-- import HGeometry.LowerEnvelope
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Vector
import HGeometry.VoronoiDiagram
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.QuickCheck.Instances ()
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "Voronoi diagram tests" $ do
  -- prop "voronoi vertex is center disk" $ \c ->
  --   voronoiVertices inputs
  it "trivial voronoi diagram" $
    voronoiVertices inputs `shouldBe` [Point2 5 5]
  runIO $ do out <- testIpe [osp|test-with-ipe/VoronoiDiagram/trivial.ipe|]
             writeIpePage [osp|/tmp/trivial.ipe|] (fromContent out)
  runIO $ do out <- testIpe [osp|test-with-ipe/VoronoiDiagram/simple.ipe|]
             writeIpePage [osp|/tmp/voronoi.ipe|] (fromContent out)

  -- goldenWith [osp|data/golden/VoronoiDiagram/|]

  -- testCases [osp|Polygon/Simple/pointInPolygon.ipe|]
  -- numericalSpec

instance Default (Point 2 R) where
  def = error "not def"

inputs :: [Point 2 R]
inputs = [origin, Point2 10 10, Point2 10 0]


testIpe      :: OsPath -> IO [IpeObject R]
testIpe inFp = do inFp' <- getDataFileName inFp
                  (points :: [Point 2 R :+ _]) <- readAllFrom inFp'

                  print $ (Point3 183.02716 93.61106 8869.99979 :: Point 3 R)
                          `onSideTest`
                          (NonVerticalHyperPlane (Vector3 282 426 (-65250)))


                  -- mapM_ print points
                  -- mapM_ (print . liftPointToPlane . view core) points
                  -- let hs = liftPointToPlane . view core <$> points
                  -- mapM_ (print . asVertex hs) $ uniqueTriplets hs


                  let vs = voronoiVertices $ (view core) <$> points
                  print "vertices"
                  mapM_ print vs
                  pure $ (map iO' points)
                         <>
                         [ iO'' v $ attr SStroke red
                         | v <- vs ]
