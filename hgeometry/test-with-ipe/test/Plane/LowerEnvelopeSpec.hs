{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Plane.LowerEnvelopeSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Golden
import           HGeometry.Box
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram
import qualified HGeometry.VoronoiDiagram as VD
import           Hiraffe.Graph.Class
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()
--------------------------------------------------------------------------------

type R = RealNumber 5






instance ( Point_ point 2 r, Fractional r, Ord r
         , Show point, Show r, IsBoxable point
         )
         => HasDefaultIpeOut (Region r point) where
  type DefaultIpeOut (Region r point) = Path
  defIO region = defIO $ case toConvexPolygonIn rect' region of
                   Left pg  -> (pg&vertices %~ view asPoint :: ConvexPolygonF NonEmpty (Point 2 r))
                   Right pg -> pg&vertices %~ view asPoint
    where
      rect' = grow 1000 $ boundingBox region

instance (Point_ point 2 r, Ord r, Num r, IsBoxable point
         ) => IsBoxable (Region r point) where
  boundingBox = \case
    Bounded pts       -> boundingBox $ toNonEmpty pts
    Unbounded _ pts _ -> boundingBox pts

grow             :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow d (Box p q) = Box (p&coordinates %~ subtract d)
                       (q&coordinates %~ (+d))



    -- $ ((uncheckedFromCCWPoints $ map (^.asPoint) vertices)
    --                      :: ConvexPolygon (Point 2 r)
    --                      )

    -- case fromPoints $ map (^.asPoint) vertices of
    --   Nothing -> error $ "could not create convex polygon?" <> show vertices
    --   Just pg -> defIO (pg :: ConvexPolygon (Point 2 r))
    -- where
    --   vertices = case region of
    --     Bounded vs        -> vs
    --     Unbounded v pts u -> let p  = NonEmpty.head pts
    --                              q  = NonEmpty.last pts
    --                              p' = p .-^ (1000 *^ v) -- TODO: clip this somewhere
    --                              q' = q .+^ (1000 *^ u) -- TODO: clip this somewhere
    --                          in q' : p' : toList pts


-- colors =

-- 1083.83267 -5893.86633 m
-- -907.76924 6277.03418 l
-- 92.23076 165.92307 l
-- 83.83267 217.24478 l
-- h

-- 1083.83267 -5893.86633 m
-- 1092.23076 915.92307 l
-- 92.23076 165.92307 l
-- 83.83267 217.24478 l
-- h

instance ( Point_ point 2 r, Fractional r, Ord r, Ord point
         , Show point, Show r
         )
         => HasDefaultIpeOut (VoronoiDiagram point) where
  type DefaultIpeOut (VoronoiDiagram point) = Group
  defIO = \case
    AllColinear colinearPts -> let sites     = toList colinearPts
                                   bisectors = toList $ separators colinearPts
                               in ipeGroup . concat $
                                  [ [ iO $ defIO b | b <- bisectors  ]
                                  , [ iO $ defIO (p^.asPoint) | p <- sites ]
                                  ]

    ConnectedVD vd          -> defIO vd

instance ( Point_ point 2 r, Fractional r, Ord r, Ord point
         , Show point, Show r, Show vertex
         , Point_ vertex 2 r, IsBoxable vertex
         )
         => HasDefaultIpeOut (VoronoiDiagram' vertex point) where
  type DefaultIpeOut (VoronoiDiagram' vertex point) = Group
  defIO = ipeGroup . zipWith render (cycle $ drop 3 basicNamedColors)
        . toList . NEMap.assocs . VD.asMap
    where
      render color (site, voronoiRegion) = iO' $ ipeGroup
                 [ iO $ defIO (site^.asPoint) ! attr SStroke  color
                 , iO $ defIO voronoiRegion   ! attr SFill    color
                                              ! attr SOpacity (Text.pack "10%")
                 ]

spec :: Spec
spec = describe "lower envelope tests" $ do
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
         testIpe [osp|degenerate.ipe|]
                 [osp|degenerate_out|]
         testIpe [osp|degenerate1.ipe|]
                 [osp|degenerate1_out|]
         testIpe [osp|degenerate2.ipe|]
                 [osp|degenerate2_out|]


-- | Build voronoi diagrams on the input points
testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let vd = voronoiDiagram $ view core <$> points
        vv = voronoiVertices $ view core <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)


-- theEdges :: PlaneGraph' (Point 2 R) h (E R) -> IpeObject' Group R
-- theEdges = ipeGroup . Map.foldMapWithKey (\v (adjs, _) ->
--                foldMap (\w -> [ iO $ defIO (ClosedLineSegment v w)
--                               ]) adjs)


-- -- build a triangulated graph from the points in the input file
-- testIpeGraph            :: OsPath -> OsPath -> Spec
-- testIpeGraph inFp outFp = do
--     (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
--       inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
--       NonEmpty.fromList <$> readAllFrom inFp'
--     case voronoiDiagram $ view core <$> points of
--       AllColinear _  -> fail "input points are colinear !?"
--       ConnectedVD vd -> do
--         let vv = voronoiVertices $ view core <$> points
--             gr = toPlaneGraph . Map.mapKeys liftPointToPlane $ VD.asMap vd -- TODO: we should flip the z no?
--             out = [ iO' points
--                   , iO' vd
--                   , iO' $ theEdges gr
--                   ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
--         goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
--                    (ipeFileGolden { name = outFp })
--                    (addStyleSheet opacitiesStyle $ singlePageFromContent out)









{-


    -- prop "voronoi vertex is center disk" $ \c ->
    --   voronoiVertices inputs
    it "vertices of a trivial voronoi diagram" $
      voronoiVertices inputs `shouldBe` [Point2 5 5]
    -- it "a trivial voronoi diagram" $
    --   voronoiDiagram inputs `shouldBe` trivialVD

    it "geometries of the trivial VD correct" $
      trivialVD^..edgeGeometries
      `shouldBe` [Left (HalfLine (Point2 5 5) (Vector2 1 0))
                 ,Left (HalfLine (Point2 5 5) (Vector2 0 (-1)))
                 ,Left (HalfLine (Point2 5 5) (Vector2 (-1) 1))
                 ]
    goldenWith [osp|data/test-with-ipe/golden/|]
               (ipeContentGolden { name = [osp|trivalVoronoi|] })
                 [ iO' inputs
                 , iO' trivialVD
                 ]


degenerateTests :: Spec
degenerateTests = describe "degnereate inputs" $ do
  it "single point diagram" $
    voronoiDiagram (NonEmpty.singleton $ Point2 1 (2 :: R))
    `shouldBe`
    AllColinear undefined  -- TODO
  it "two point diagram" $
    voronoiDiagram (NonEmpty.fromList [Point2 1 (2 :: R), Point2 3 2])
    `shouldBe`
    AllColinear undefined -- TODO
  it "multiple parallel point diagram" $
    voronoiDiagram (NonEmpty.fromList [ Point2 x (2 :: R)
                                      | x <- fromInteger <$> [1..10]
                                      ])
    `shouldBe`
    AllColinear undefined -- TODO

             -- goldenWith [osp|data/test-with-ipe/golden/|]
  --            (ipeContentGolden { name = [osp|voronoi|] })
  --              [ iO' inputs
  --              ]
  --              , iO' trivialVD





instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r
         , Show r, Show point
         )
         => HasDefaultIpeOut (VoronoiDiagram point) where
  type DefaultIpeOut (VoronoiDiagram point) = Group
  defIO = \case
    AllColinear _pts -> ipeGroup []
    ConnectedVD vd  -> defIO vd


instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r
         , Show r, Show point
         )
         => HasDefaultIpeOut (VoronoiDiagram' point) where
  type DefaultIpeOut (VoronoiDiagram' point) = Group
  defIO vd = ipeGroup $ vd^..edgeGeometries.to render
    where
      bRect = boundingBox $ defaultBox :| [grow 1 $ boundingBox vd]
      render = \case
        Left hl   -> iO $ ipeHalfLineIn bRect hl
        Right seg -> iO' seg

inputs :: [Point 2 R]
inputs = [origin, Point2 10 10, Point2 10 0]


trivialVD :: VoronoiDiagram' (Point 2 R)
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
    planes = map (\p -> liftPointToPlane p :+ p) inputs
    (h1,h2,h3) = case planes of
                   [h1',h2',h3'] -> (h1',h2',h3')
                   _             -> error "absurd"
  -- order of the planes is incorrect, as is the z-coord.

-}
