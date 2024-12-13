{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module VoronoiDiagram.VoronoiSpec
  ( spec
  ) where

import           Control.Lens
import           Golden
-- import HGeometry.Combinatorial.Util
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.Plane.LowerEnvelope
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           HGeometry.Number.Real.Rational
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Box
import           HGeometry.Line.General
import           HGeometry.HalfLine
import           HGeometry.VoronoiDiagram
import           Ipe
import           Ipe.Color
import           Test.Hspec.WithTempFile
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()
import           Plane.LowerEnvelopeSpec () -- imports the ipe instances for Voronoi Diagram
import           HGeometry.Sequence.Alternating (Alternating(..))
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "Voronoi diagram tests" $ do
    -- prop "voronoi vertex is center disk" $ \c ->
    --   voronoiVertices inputs
    it "vertices of a trivial voronoi diagram" $
      voronoiVertices inputs `shouldBe` (Set.fromList [Point2 5 5])
    -- it "a trivial voronoi diagram" $
    --   voronoiDiagram inputs `shouldBe` trivialVD

    -- it "geometries of the trivial VD correct" $
    --   trivialVD^..edgeGeometries
    --   `shouldBe` [Left (HalfLine (Point2 5 5) (Vector2 1 0))
    --              ,Left (HalfLine (Point2 5 5) (Vector2 0 (-1)))
    --              ,Left (HalfLine (Point2 5 5) (Vector2 (-1) 1))
    --              ]


    goldenWith [osp|data/test-with-ipe/VoronoiDiagram/|]
               (ipeContentGolden { name = [osp|trivialVoronoi|] })
                 [ iO' inputs
                 , iO' trivialVD
                 ]

    degenerateTests

    testIpe [osp|trivial.ipe|]
            [osp|trivial_out|]
    testIpe [osp|simplest.ipe|]
            [osp|simplest_out|]
    testIpe [osp|simpler.ipe|]
            [osp|simpler_out|]
    testIpe [osp|simple.ipe|]
            [osp|simple_out|]
    testIpe [osp|simple1.ipe|]
            [osp|simple1_out|]
    testIpe [osp|foo.ipe|]
            [osp|foo_out|]
    testIpe [osp|colinear.ipe|]
            [osp|colinear_out|]
    testIpe [osp|pair.ipe|]
            [osp|pair_out|]

degenerateTests :: Spec
degenerateTests = describe "degnereate inputs" $ do
  it "single point diagram" $
    voronoiDiagram (NonEmpty.singleton $ Point2 1 (2 :: R))
    `shouldBe`
    AllColinear (Alternating (Point2 1 2) mempty)
  it "two point diagram" $
    voronoiDiagram (NonEmpty.fromList [Point2 1 (2 :: R), Point2 3 2])
    `shouldBe`
    AllColinear (Alternating (Point2 1 2) (Vector.fromList [(VerticalLineThrough 2, Point2 3 2)]))
  it "multiple parallel point diagram" $
    voronoiDiagram (NonEmpty.fromList [ Point2 x (2 :: R)
                                      | x <- fromInteger <$> [1..10]
                                      ])
    `shouldBe`
    AllColinear (Alternating (Point2 1 2) . Vector.fromList $
                 [(VerticalLineThrough 1.5,Point2 2 2)
                 ,(VerticalLineThrough 2.5,Point2 3 2)
                 ,(VerticalLineThrough 3.5,Point2 4 2)
                 ,(VerticalLineThrough 4.5,Point2 5 2)
                 ,(VerticalLineThrough 5.5,Point2 6 2)
                 ,(VerticalLineThrough 6.5,Point2 7 2)
                 ,(VerticalLineThrough 7.5,Point2 8 2)
                 ,(VerticalLineThrough 8.5,Point2 9 2)
                 ,(VerticalLineThrough 9.5,Point2 10 2)])


  it "buggy four points diagram" $
    numRegions (voronoiDiagram bug)
    `shouldBe`
    Just 4


numRegions = \case
  AllColinear _   -> Nothing
  ConnectedVD env -> Just . length . HGeometry.VoronoiDiagram.asMap $ env

             -- goldenWith [osp|data/test-with-ipe/golden/|]
  --            (ipeContentGolden { name = [osp|voronoi|] })
  --              [ iO' inputs
  --              ]
  --              , iO' trivialVD



grow             :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow d (Box p q) = Box (p&coordinates %~ subtract d)
                       (q&coordinates %~ (+d))


-- instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r
--          , Show r, Show point
--          )
--          => HasDefaultIpeOut (VoronoiDiagram point) where
--   type DefaultIpeOut (VoronoiDiagram point) = Group
--   defIO = \case
--     AllColinear _pts -> ipeGroup []
--     ConnectedVD vd  -> defIO vd


-- instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r
--          , Show r, Show point
--          )
--          => HasDefaultIpeOut (VoronoiDiagram' point) where
--   type DefaultIpeOut (VoronoiDiagram' point) = Group
--   defIO vd = ipeGroup $ vd^..edgeGeometries.to render
--     where
--       bRect = boundingBox $ defaultBox :| [grow 1 $ boundingBox vd]
--       render = \case
--         Left hl   -> iO $ ipeHalfLineIn bRect hl
--         Right seg -> iO' seg



inputs :: NonEmpty (Point 2 R)
inputs = NonEmpty.fromList [origin, Point2 10 10, Point2 10 0]

trivialVD :: VoronoiDiagram (Point 2 R)
trivialVD = voronoiDiagram inputs


bug = NonEmpty.fromList $
      [ Point2 10 0
      , Point2 0  10
      , Point2 30 10
      , Point2 10 30
      ]



  -- VoronoiDiagram $ LowerEnvelope vInfty (Seq.fromList [bv])
  -- where
  --   vInfty = UnboundedVertex $ Seq.fromList [Edge 1 h2 h3
  --                                           ,Edge 1 h3 h1
  --                                           ,Edge 1 h1 h2
  --                                           ]
  --   bv = Vertex (Point3 5 5 0)
  --               (Set.fromList planes)
  --               (Seq.fromList $
  --                [ Edge 0 h2 h1
  --                , Edge 0 h3 h2
  --                , Edge 0 h1 h3
  --                ]
  --               )
  --   planes = map (\p -> liftPointToPlane p :+ p) inputs
  --   (h1,h2,h3) = case planes of
  --                  [h1',h2',h3'] -> (h1',h2',h3')
  --                  _             -> error "absurd"
  -- -- order of the planes is incorrect, as is the z-coord.


testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let vd = voronoiDiagram $ (view core) <$> points
        vv = voronoiVertices $ (view core) <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/VoronoiDiagram/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)
