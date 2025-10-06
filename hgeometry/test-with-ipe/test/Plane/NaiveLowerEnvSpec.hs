{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances  #-}
module Plane.NaiveLowerEnvSpec
  ( spec
  ) where

import           HGeometry.Cyclic
import           Control.Lens hiding (IsEmpty, IsNonEmpty)
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Golden
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Clipped
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Triangle
import           HGeometry.Vector
import qualified HGeometry.VoronoiDiagram as VD
import           HGeometry.VoronoiDiagram.Clipped
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5


--




-- type ClippedMinimizationDiagram' r plane = NEMap plane (ClippedMDCell r (MDVertex r plane))

-- type ClippedMDCell' r vertex =
--   PossiblyDegenerateSimplePolygon (OriginalOrExtra vertex (Point 2 r))
--                                   (ClippedBoundedRegion r vertex (Point 2 r))


--------------------------------------------------------------------------------


spec :: Spec
spec = describe "lower envelope in bounded region" $ do
         myTest

         testIpe [osp|trivial.ipe|]
                 [osp|trivial_clipped_out|]
         testIpe [osp|simplest.ipe|]
                 [osp|simplest_clipped_out|]
         testIpe [osp|simpler.ipe|]
                 [osp|simpler_clipped_out|]
         testIpe [osp|simple.ipe|]
                 [osp|simple_clipped_out|]
         testIpe [osp|simple1.ipe|]
                 [osp|simple1_clipped_out|]
         testIpe [osp|foo.ipe|]
                 [osp|foo_clipped_out|]
         testIpe [osp|degenerate.ipe|]
                 [osp|degenerate_clipped_out|]
         testIpe [osp|degenerate1.ipe|]
                 [osp|degenerate1_clipped_out|]
         testIpe [osp|degenerate2.ipe|]
                 [osp|degenerate2_clipped_out|]


-- | Build voronoi diagrams on the input points
testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let tri = Triangle origin (Point2 1000 0) (Point2 0 500)
        vd = voronoiDiagramIn tri $ view core <$> points
        -- vd = rVoronoiDiagram $ view core <$> points
        vv = VD.voronoiVertices $ view core <$> points
        out = [ iO' points
              , iO' vd
              , iO $ defIO tri
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    -- runIO $ print vd
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)




instance ( Point_ vertex 2 r, Point_ extra 2 r, Fractional r, Ord r
         , Show r
         )
         => HasDefaultIpeOut (ClippedMDCell'' r vertex extra) where
  type DefaultIpeOut (ClippedMDCell'' r vertex extra) = Path
  defIO (ClippedMDCell cell) = case cell of
    ActualPolygon cell' -> defIO (cell'&vertices %~ (^.asPoint)
                                    :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 r)
                                 )
    _                   -> error "defIO for clippedMDCell rendering segment or point not defined"


-- instance ( Fractional r, Ord r
--          , Show r
--          )
--          => HasDefaultIpeOut (ClippedMinimizationDiagram' r plane) where
--   type DefaultIpeOut (ClippedMinimizationDiagram' r plane) = Group
--   defIO (ClippedMinimizationDiagram env) = ipeGroup $ foldMap ((:[]) . iO . defIO) env


instance ( Fractional r, Ord r, Point_ point 2 r
         , Show r
         )
         => HasDefaultIpeOut (ClippedVoronoiDiagram point) where
  type DefaultIpeOut (ClippedVoronoiDiagram point) = Group
  defIO = ipeGroup . zipWith render (cycle $ drop 3 basicNamedColors)
        . toList . NEMap.assocs . view _ClippedVoronoiDiagram
    where
      render color (site, voronoiRegion) = iO' $ ipeGroup
                 [ iO $ defIO (site^.asPoint) ! attr SStroke  color
                                              ! attr SSize    large
                 , iO $ defIO voronoiRegion   ! attr SFill    color
                                              ! attr SOpacity (Text.pack "10%")
                 ]

large :: IpeSize r
large = IpeSize $ Named (Text.pack "large")

    -- NEMap.foldMapWithKey (\p cell -> [ iO $ defIO (p^.asPoint)
    --                                  , iO $ defIO cell
    --                                  ]
    --                      ) m



myTest = describe "intersection tests" $ do
    let tri = Triangle origin (Point2 1000 0) (Point2 0 500)
    it "triangle conex poly intersection "  $
      (tri `intersects` convexPoly) `shouldBe` True
    it "triangle unbounded conex poly intersection "  $
      (tri `intersects` unboundedPoly) `shouldBe` True

-- convexPoly :: ConvexPolygon (Point 2 R)
convexPoly :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 R)
convexPoly = fromMaybe (error "absurd") $ fromPoints $ NonEmpty.fromList
  [ Point2 281.99      1636.18
  , Point2 (-662.455) 1636.18
  , Point2 (-662.455) (-363.818)
  , Point2 (-305.312) (-363.818)
  , Point2 337.545    636.182
  ]

unboundedPoly :: UnboundedConvexRegion (Point 2 R)
unboundedPoly = Unbounded (Vector2 1 1.55555)
                          (NonEmpty.fromList [Point2 337.54545 636.18181])
                          (Vector2 (-1) 18)

-- (MDVertex {_location = Point3 337.54545~ 636.18181~ (-497088), _definers = Definers ((NonVerticalHyperPlane [-960,-1344,681984] :+ Point2 480 672) :| [NonVerticalHyperPlane [-384,-1312,467200] :+ Point2 192 656,NonVerticalHyperPlane [-832,-1024,435200] :+ Point2 416 512])} :| []) (Vector2 (-1) 18);
