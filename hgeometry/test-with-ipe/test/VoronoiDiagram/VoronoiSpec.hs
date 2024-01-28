{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module VoronoiDiagram.VoronoiSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Default.Class
import           Golden
-- import HGeometry.Combinatorial.Util
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LowerEnvelope
import           HGeometry.LowerEnvelope.AdjListForm
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram
import           Ipe
import           Ipe.Color
import           Test.Hspec.WithTempFile
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "Voronoi diagram tests" $ do
  -- prop "voronoi vertex is center disk" $ \c ->
  --   voronoiVertices inputs
  it "vertices of a trivial voronoi diagram" $
    voronoiVertices inputs `shouldBe` [Point2 5 5]
  -- it "a trivial voronoi diagram" $
  --   voronoiDiagram inputs `shouldBe` trivialVD

  runIO $ do out <- testIpe [osp|test-with-ipe/VoronoiDiagram/trivial.ipe|]
             writeIpePage [osp|/tmp/trivial.ipe|] (fromContent out)
  runIO $ do out <- testIpe [osp|test-with-ipe/VoronoiDiagram/simple.ipe|]
             writeIpePage [osp|/tmp/voronoi.ipe|] (fromContent out)

  it "geometries" $
    (trivialVD^..edgeGeometries `shouldBe` [])
    -- nonsense, but

  -- let [Left hl1,Left hl2,Left hl3] = trivialVD^..edgeGeometries
  -- runIO $ print hl1


  -- it "hl1" $
  --   iO' hl1 `shouldBe` undefined
  -- it "hl2" $
  --   iO' hl1 `shouldBe` undefined
  -- it "hl3" $
  --   iO' hl2 `shouldBe` undefined


  goldenWith [osp|data/test-with-ipe/golden/|]
             (ipeContentGolden { name = [osp|voronoi|] })
               [ iO' inputs
               , iO' trivialVD
               ]


instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r, Default point
         , Show r, Show point
         )
         => HasDefaultIpeOut (VoronoiDiagram point) where
  type DefaultIpeOut (VoronoiDiagram point) = Group
  defIO vd = ipeGroup $ vd^..edgeGeometries.to iO'



-- edgeSegments :: IndexedTraversal' (Edge )
-- edgeSegments =


  -- goldenWith

  -- [osp|data/golden/VoronoiDiagram/|]

  -- testCases [osp|Polygon/Simple/pointInPolygon.ipe|]
  -- numericalSpec



instance Default (Point 2 R) where
  def = error "not def"

inputs :: [Point 2 R]
inputs = [origin, Point2 10 10, Point2 10 0]


trivialVD :: VoronoiDiagram (Point 2 R)
trivialVD = VoronoiDiagram $ LowerEnvelope vInfty (Seq.fromList [bv])
  where
    vInfty = UnboundedVertex $ Seq.fromList [Edge 1 h2 h3
                                            ,Edge 1 h3 h1
                                            ,Edge 1 h1 h2
                                            ]
    bv = Vertex (Point3 5 5 0)
                (Set.fromList planes)
                (Seq.fromList $
                 [ Edge 0 h2 h1
                 , Edge 0 h3 h2
                 , Edge 0 h1 h3
                 ]
                )
    planes@[h1,h2,h3] = map (\p -> liftPointToPlane p :+ p) inputs
  -- order of the planes is incorrect, as is the z-coord.

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
