{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Plane.LowerEnvelopeSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Golden
import           HGeometry.Box
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r)
         => HasDefaultIpeOut (Region r point) where
  type DefaultIpeOut (Region r point) = Path
  defIO region = case fromPoints $ map (^.asPoint) vertices of
      Nothing -> error "could not create convex polygon?"
      Just pg -> defIO (pg :: ConvexPolygon (Point 2 r))
    where
      vertices = case region of
        Bounded vs        -> vs
        Unbounded u pts v -> let p  = NonEmpty.head pts
                                 q  = NonEmpty.last pts
                                 p' = p .-^ (1000 *^ u) -- TODO: clip this somewhere
                                 -- observe that the vectors point away from
                                 -- the vertex.
                                 q' = q .+^ (1000 *^ v) -- TODO: clip this somewhere
                             in q' : p' : toList pts


-- colors =

instance ( Point_ point 2 r, Fractional r, Ord r)
         => HasDefaultIpeOut (MinimizationDiagram r point) where
  type DefaultIpeOut (MinimizationDiagram r point) = Group
  defIO = ipeGroup . zipWith render (cycle $ drop 3 basicNamedColors) . Map.assocs
    where
      render color (site, voronoiRegion) = iO' $ ipeGroup
                 [ iO $ defIO (site^.asPoint) ! attr SStroke  color
                 , iO $ defIO voronoiRegion   ! attr SFill    color
                                              ! attr SOpacity (Text.pack "10%")
                 ]

spec :: Spec
spec = describe "lower envelope tests" $ do
         -- testIpe [osp|trivial.ipe|]
         --         [osp|trivial_out|]
         testIpe [osp|simplest.ipe|]
                 [osp|simplest_out|]
         testIpe [osp|simpler.ipe|]
                 [osp|simpler_out|]
         testIpe [osp|simple.ipe|]
                 [osp|simple_out|]
         -- testIpe [osp|simple1.ipe|]
         --         [osp|simple1_out|]
         -- testIpe [osp|foo.ipe|]
         --         [osp|foo_out|]


-- | Computes the vertex form of the upper envelope. The z-coordinates are still flipped.

type VoronoiDiagram' r point = MinimizationDiagram r point

voronoiDiagram' :: ( Point_ point 2 r, Functor f, Ord point
                   , Ord r, Fractional r, Foldable1 f
                   , Show point, Show r


                   ) => f point -> VoronoiDiagram' r point
voronoiDiagram' = Map.mapKeysMonotonic (view extra)
                . bruteForceLowerEnvelope
                . fmap (\p -> flipZ (liftPointToPlane p) :+ p)
  where
    flipZ :: Num r => Plane r -> Plane r
    flipZ = over (hyperPlaneCoefficients.traverse) negate

voronoiVertices :: ( Point_ point 2 r, Functor f, Ord point
                   , Ord r, Fractional r, Foldable1 f
                   , Show point, Show r
                   , Ord point
                   ) => f point -> Set (Point 2 r)
voronoiVertices = foldMap (\case
                              Bounded pts       -> Set.fromList pts
                              Unbounded _ pts _ -> Set.fromList (NonEmpty.toList pts)
                          ) . voronoiDiagram'


testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let vd = voronoiDiagram' $ (view core) <$> points
        vv = voronoiVertices $ (view core) <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)
     -- FIXME: use data/test-with-ipe/VoronoiDiagram/ as input and data/test-with-ipe/Plane/LowerEnvelope
      -- as output dir


-- outputFile' <- addStyleSheetFrom "../hgeometry-ipe/resources/opacities.isy" outputFile

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



grow             :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow d (Box p q) = Box (p&coordinates %~ subtract d)
                       (q&coordinates %~ (+d))


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







-- testIpe      :: OsPath -> IO [IpeObject R]
-- testIpe inFp = do inFp' <- getDataFileName inFp
--                   (points :: [Point 2 R :+ _]) <- readAllFrom inFp'

--                   print $ (Point3 183.02716 93.61106 8869.99979 :: Point 3 R)
--                           `onSideTest`
--                           (NonVerticalHyperPlane (Vector3 282 426 (-65250)))


--                   -- mapM_ print points
--                   -- mapM_ (print . liftPointToPlane . view core) points
--                   -- let hs = liftPointToPlane . view core <$> points
--                   -- mapM_ (print . asVertex hs) $ uniqueTriplets hs

--                   let vv = voronoiVertices $ (view core) <$> points
--                   let vd = voronoiDiagram $ (view core) <$> points
--                   print $ vd


--                   -- print "vertices"
--                   -- mapM_ print vs
--                   pure $ [ iO' points
--                          , iO' vd
--                          ] <> [ iO'' v $ attr SStroke red | v <- vv ]


--                     -- $ (map iO' points)
--                     --      -- <> [iO' vd]
--                     -- <>
-}
