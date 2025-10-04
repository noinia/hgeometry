{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Plane.LowerEnvelopeSpec
  -- ( spec
  -- ) where
  where


import           Control.Lens hiding (below)
import           Data.Foldable
import           Data.Bifoldable
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as Text
import           Golden
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.Polygon.Convex.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Plane.LowerEnvelope.Clipped (foldMapVertices, ClippedMDCell, ClippedMDCell''(..))
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Triangle
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram
import qualified HGeometry.VoronoiDiagram as VD
import           Hiraffe.Graph.Class
import           Ipe
import           Ipe.Color
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           HGeometry.Combinatorial.Util
import           HGeometry.Polygon.Convex.Unbounded

import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5


rVoronoiDiagram :: ( Point_ point 2 r, Functor f, Ord point
                   , Ord r, Fractional r, Foldable1 f
                   , Show point, Show r
                   ) => f point -> VoronoiDiagram point ()
rVoronoiDiagram = voronoiDiagramWith' $ Randomized.computeVertexForm (mkStdGen 1)

--------------------------------------------------------------------------------


instance ( Point_ point 2 r, Fractional r, Ord r
         , Show point, Show r, IsBoxable point
         )
         => HasDefaultIpeOut (Region r point) where
  type DefaultIpeOut (Region r point) = Path
  defIO region = defIO $ case toConvexPolygonIn rect' region of
                   Left pg  -> (pg&vertices %~ view asPoint :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 r))
                   Right pg -> pg&vertices %~ view asPoint
    where
      rect' = grow 1000 $ boundingBox region

instance (Point_ point 2 r, Ord r, Num r, IsBoxable point
         ) => IsBoxable (Region r point) where
  boundingBox = \case
    BoundedRegion convex                -> boundingBox convex
    UnboundedRegion (Unbounded _ pts _) -> boundingBox pts

grow             :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow d (Box p q) = Box (p&coordinates %~ subtract d)
                       (q&coordinates %~ (+d))




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
         , Show point, Show r, Show vtxData
         )
         => HasDefaultIpeOut (VoronoiDiagram_ r point vtxData) where
  type DefaultIpeOut (VoronoiDiagram_ r point vtxData) = Group
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
         randomizedSameAsBruteForce

         -- testIpe [osp|trivial.ipe|]
         --         [osp|trivial_out|]
         -- testIpe [osp|simplest.ipe|]
         --         [osp|simplest_out|]
         -- testIpe [osp|simpler.ipe|]
         --         [osp|simpler_out|]
         -- testIpe [osp|simple.ipe|]
         --         [osp|simple_out|]
         -- testIpe [osp|simple1.ipe|]
         --         [osp|simple1_out|]
         -- testIpe [osp|foo.ipe|]
         --         [osp|foo_out|]
         -- testIpe [osp|degenerate.ipe|]
         --         [osp|degenerate_out|]
         -- testIpe [osp|degenerate1.ipe|]
         --         [osp|degenerate1_out|]
         -- testIpe [osp|degenerate2.ipe|]
         --         [osp|degenerate2_out|]

         prop "withConflictList correct" $
           withConflictListTest
         prop "withExtraConflictLists correct" $
           \(Positive r') planes -> let r = max r' 3 in
             withExtraConflictListTest r planes
         prop "original means definer" $
           \(Positive r') planes -> let r = max r' 3 in
             originalMeansDefiner r planes
         prop "original vertices are original" $ do
           \(Positive r') tri planes -> let r = max r' 3 in
             allOriginal' 3 tri planes

         runIO $ do
           for_ [ NonVerticalHyperPlane $ fromList' [-1,-1,1]
                , NonVerticalHyperPlane $ fromList' [-0.5,-2,1.5]
                , NonVerticalHyperPlane $ fromList' [2,1,0]
                ] (putStrLn . showPlaneEquation)

         prop "bug" $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               planes = NESet.fromList . NonEmpty.fromList . map MyPlane $
                       [ NonVerticalHyperPlane $ fromList' [-1,-1,1]
                       , NonVerticalHyperPlane $ fromList' [-0.5,-2,1.5]
                       , NonVerticalHyperPlane $ fromList' [2,1,0]
                       ]
           withExtraConflictListTest 3 planes tri
           -- for some reason we get an inccorrect original vertex !?
           -- somehow we generate a vertex at (2,1.5) which is not actually an original
           -- vertex. Not sure how that went

         prop "bug origMeansDefiner" $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               planes = NESet.fromList . NonEmpty.fromList . map MyPlane $
                       [ NonVerticalHyperPlane $ fromList' [-1,-1,1]
                       , NonVerticalHyperPlane $ fromList' [-0.5,-2,1.5]
                       , NonVerticalHyperPlane $ fromList' [2,1,0]
                       ]
           originalMeansDefiner 3 planes tri


         prop "bug, original vertices are original" $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               planes = NESet.fromList . NonEmpty.fromList . map MyPlane $
                       [ NonVerticalHyperPlane $ fromList' [-1,-1,1]
                       , NonVerticalHyperPlane $ fromList' [-0.5,-2,1.5]
                       , NonVerticalHyperPlane $ fromList' [2,1,0]
                       ]
           allOriginal' 3 tri planes

         prop "original vertices are really original vertices (bounded)" $
           allOriginal @(ConvexPolygon (Point 2 R))

         prop "original vertices are really original vertices (unbounded)" $
           allOriginal @(UnboundedConvexRegion (Point 2 R))

         specUnbounded


-- TODO: move these tests to some module about Unbounded
specUnbounded :: Spec
specUnbounded = describe "fromUnbounded correct" $ do
         prop "boundedFromVertices that intersect triangle are orig vertices " $
           verifyBoundedFromVertices

         prop "bug, boundedFromVertices that intersect triangle are orig vertices " $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               reg = Unbounded (Vector2 (-1) (-0.5))
                               (NonEmpty.singleton (Point2 0 0.5))
                               (Vector2 1 (-1.5))
           verifyBoundedFromVertices tri reg

         prop "bug, intersection" $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               reg = Unbounded (Vector2 (-1) (-0.5))
                               (NonEmpty.singleton (Point2 0 0.5))
                               (Vector2 1 (-1.5))
           allOriginal @(UnboundedConvexRegion (Point 2 R)) reg tri




         -- runIO $ do
         --   let tri :: Triangle (Point 2 R)
         --       tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
         --       reg :: UnboundedConvexRegion (Point 2 R)
         --       reg = Unbounded (Vector2 (-1) (-0.5))
         --                       (NonEmpty.singleton (Point2 0 0.5))
         --                       (Vector2 1 (-1.5))
         --   print tri
         --   print reg
         --   print "=======> via =========> "
         --   print $ toBoundedFrom tri reg
         --   print "=======> intersects in =========> "
         --   print $ tri `intersect` reg



-- | Make sure that the vertices of the bounded convex region we create that
-- intersect the triagnle are actually original vertices of the convex region as well.
verifyBoundedFromVertices         :: Triangle (Point 2 R) -> UnboundedConvexRegion (Point 2 R)
                                  -> Every
verifyBoundedFromVertices tri reg = foldMapOf vertices (Every . check) $ toBoundedFrom tri reg
  where
    check v | v `intersects` tri = counterexample (show v) -- if v lies inside it should be
                                 $ isOrigVertex v
             | otherwise         = property True
    isOrigVertex v = elemOf vertices v reg



-- moreover: I think the main issue should be in  the Unbounded.toBoundedFrom function
-- which displaces vertices


--------------------------------------------------------------------------------





--                    , _definers = Definers (NonVerticalHyperPlane [-0.5,-2,1.5] :| [NonVerticalHyperPlane [2,1,0],NonVerticalHyperPlane [-1,-1,1]]), _vertexData = (Definers (NonVerticalHyperPlane [-0.5,-2,1.5] :| [NonVerticalHyperPlane [2,1,0],NonVerticalHyperPlane [-1,-1,1]]),fromList [])} :| []) (Vector2 1 (-1.5)))


-- ,"-> "

-- ,Just (ClippedMDCell (ActualPolygon (ConvexPolygon [Extra (Point2 1.8 1.4),Extra (Point2 1 (-1)),Extra (Point2 2 0),Original (MDVertex {_location = Point3 2 1.5 0.5, _definers = Definers (NonVerticalHyperPlane [-0.5,-2,1.5] :| [NonVerticalHyperPlane [2,1,0],NonVerticalHyperPlane [-1,-1,1]]), _vertexData = (Definers (NonVerticalHyperPlane [-0.5,-2,1.5] :| [NonVerticalHyperPlane [2,1,0],NonVerticalHyperPlane [-1,-1,1]]),fromList [])})]))))





-- | Check if all original vertices of a triangle and a polygon are really original
allOriginal          :: ( IsIntersectableWith (Triangle (Point 2 R)) poly
                          , Intersection (Triangle (Point 2 R)) poly ~
                             Maybe (PossiblyDegenerateSimplePolygon
                               (OriginalOrExtra (Point 2 R) (Point 2 R))
                               (poly' (OriginalOrExtra (Point 2 R) (Point 2 R))))
                        , Foldable poly'
                        , Show (poly' (OriginalOrExtra (Point 2 R) (Point 2 R)))
                        , HasVertices poly poly
                        , Vertex poly ~ Point 2 R
                        ) => poly -> Triangle (Point 2 R) -> Every
allOriginal poly tri = case tri `intersect` poly of
    Nothing  -> discard
    Just res -> bifoldMap f (foldMap f) res
      where
        f :: OriginalOrExtra (Point 2 R) (Point 2 R) -> Every
        f = Every . \case
          Extra    _ -> property True
          Original v -> counterexample (show res) . counterexample (show v) . property
                      $ v `elem` origVertices

        origVertices = tri^..vertices <> poly^..vertices



allOriginal'            :: Int -> Triangle (Point 2 R) -> NESet.NESet MyPlane -> Every
allOriginal' r tri planes =
    case Randomized.withConflictLists remaining (BruteForce.computeVertexForm rNet) of
      NEMap.IsEmpty                 -> discard
      NEMap.IsNonEmpty verticesRNet -> foldMap check $ fromVertexFormIn tri rNet verticesRNet
  where
    (rNet, remaining) = Randomized.takeSample r planes
    check res = foldMapVertices f res
      where
        f :: OriginalOrExtra (MDVertex R _ _) extra -> Every
        f = Every . \case
          Extra    _ -> property True
          Original v -> counterexample (show res) . counterexample (show v) . property
                      $ (v^.asPoint) `elem` origVertices

        origVertices = tri^..vertices <> foldMapVertices (\p -> [p^.asPoint]) res





-- seems like intersecting triangles and convex polygons incorrectly produces some 'Original' vertices?

         -- it "definers test" $ do
         --   let hs = Three (NonVerticalHyperPlane $ fromList' [-1,-1,1])
         --                  (NonVerticalHyperPlane $ fromList' [-0.5,-2,1.5])
         --                  (NonVerticalHyperPlane $ fromList' [2,1,0])
         --   intersectionPoint hs `shouldNotBe` Just (Point3 2 1.5 0.5)
         --   -- OK: this test verfies that our vertex location 2 1.5. 0.5 is incorrect
         --

-- | Check that every original vertex is the intersection of its definers
originalMeansDefiner              :: Int -> NESet.NESet MyPlane -> Triangle (Point 2 R) -> Every
originalMeansDefiner r planes tri =
    case Randomized.withConflictLists remaining (BruteForce.computeVertexForm rNet) of
      NEMap.IsEmpty                 -> discard
      NEMap.IsNonEmpty verticesRNet -> ifoldMap check $ fromVertexFormIn tri rNet verticesRNet
  where
    (rNet, remaining) = Randomized.takeSample r planes

    -- verify that for all vertices, if it is an original vertex, it has the correct definers
    check                        :: (HasDefiners vertexData MyPlane, Show vertexData)
                                 => MyPlane -> ClippedMDCell R MyPlane vertexData
                                 -> Every
    check h (ClippedMDCell cell) = case cell of
      DegenerateVertex v       -> Every $ check' v
      DegenerateEdge e         -> Every $ check' (e^.start) .&&.check' (e^.end)
      ActualPolygon convexCell -> foldMapOf vertices ( Every
                                                     . counterexample (show cell)
                                                     . counterexample (show h)
                                                     . check'
                                                     ) convexCell

    -- verify that for an origianl vertex it has the right definers
    check' :: Show vertexData
           => OriginalOrExtra (MDVertex R MyPlane vertexData) (Point 2 R) -> Property
    check' = \case
      Original v -> counterexample (show v) $
                    case toList $ definersOf v of
                       (a:b:c:_) -> intersectionPoint (Three a b c) === Just (v^.location)
                       _         -> error "originalMeansDefiner. absurd"
      Extra _    -> property True

-- | verify that 'withConflictLists' is correct
withConflictListTest                     :: NESet.NESet MyPlane -> Positive Int -> Bool
withConflictListTest planes (Positive r) = iall correctConflictLists $
      Randomized.withConflictLists remaining (BruteForce.computeVertexForm rNet)
  where
    (rNet, remaining) = Randomized.takeSample r planes
    below     :: Point_ point 3 R => point -> MyPlane -> Bool
    below v h = verticalSideTest v h == GT
    correctConflictLists v (_,conflicts) = conflicts == NESet.filter (below v) planes



instance FoldableWithIndex k (NEMap.NEMap k) where
  ifoldMap = NEMap.foldMapWithKey


-- verifyAllTriangles ::



-- | verify that withExtraConflictListTest is correct
withExtraConflictListTest              :: Int -> NESet.NESet MyPlane -> Triangle (Point 2 R)
                                       -> Every
withExtraConflictListTest r planes tri =
    case Randomized.withConflictLists remaining (BruteForce.computeVertexForm rNet) of
      NEMap.IsEmpty                 -> discard
      NEMap.IsNonEmpty verticesRNet ->
        ifoldMap check . Randomized.withExtraConflictLists (NESet.toSet planes)
                       . fromVertexFormIn tri rNet $ verticesRNet
  where
    (rNet, remaining) = Randomized.takeSample r planes

    -- verify that for all vertices of the clipped cell, the conflict list is correct
    check                        :: Show c => MyPlane
                                 -> ClippedMDCell'' R (MDVertex R MyPlane (c, Set.Set MyPlane))
                                                      (Point 2 R :+ (_, Set.Set MyPlane))
                                 -> Every
    check h (ClippedMDCell cell) = case cell of
      DegenerateVertex v       -> Every $ check' h v
      DegenerateEdge e         -> Every $ check' h (e^.start) .&&.check' h (e^.end)
      ActualPolygon convexCell -> foldMapOf vertices (check' h) convexCell

    -- verify that for a given vertex its conflict list is correct
    check'   :: Show c => MyPlane
             -> OriginalOrExtra (MDVertex R MyPlane (c, Set.Set MyPlane))
                                (Point 2 R :+ (_, Set.Set MyPlane))
             -> Every
    check' h = Every . \case
      Original v                           -> let conflicts = v^.vertexData._2
                                                  z         = v^.location.zCoord
                                               in counterexample ("orig " <> show (v^.location
                                                                                    ,evalAt v h
                                                                                    , v^.vertexData._1
                                                                                  )) $
                                                  conflicts === NESet.filter (below v z) planes
      Extra (v :+ (_extraDefs, conflicts)) -> let z = evalAt v h
                                              in counterexample ("extra" <> show v) $
                                                 conflicts === NESet.filter (below v z) planes



    below                   :: Point_ point 2 R => point -> R -> MyPlane -> Bool
    below (Point2_ x y) z h = verticalSideTest (Point3 x y z) h == GT


-- | Build voronoi diagrams on the input points
testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let --vd = voronoiDiagram $ view core <$> points
        vd = rVoronoiDiagram $ view core <$> points
        vv = voronoiVertices $ view core <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)


-- testClipVoronoi            :: OsPath -> OsPath -> Spec
-- testClipVoronoi inFp outFp = do
--     (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
--       inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
--       NonEmpty.fromList <$> readAllFrom inFp'
--     let vd'    = asMap $ voronoiDiagram $ view core <$> points
--         theMap = undefined
--         -- theMap = NEMap.mapWithKey (\p reg -> )
--         -- vv = voronoiVertices $ view core <$> points

--         a  = Point2 0    0
--         b  = Point2 0    2000
--         c  = Point2 2000 1000
--         vd = fromVertexFormIn (Triangle a b c) theMap


--         -- vd = rVoronoiDiagram $ view core <$> points

--         out = [ iO' points
--               , iO' vd
--               ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
--     goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
--                (ipeFileGolden { name = outFp })
--                (addStyleSheet opacitiesStyle $ singlePageFromContent out)





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








--------------------------------------------------------------------------------

newtype MyPlane = MyPlane (Plane R)
  deriving newtype (Show,Eq,Ord)

asPlane :: Iso' MyPlane (Plane R)
asPlane = coerced

type instance NumType MyPlane   = R
type instance Dimension MyPlane = 3

instance HyperPlane_ MyPlane 3 R
instance NonVerticalHyperPlane_  MyPlane 3 R where
  hyperPlaneCoefficients = asPlane.hyperPlaneCoefficients

instance Arbitrary MyPlane where
  arbitrary = do h <- arbitrary `suchThat` allOf (hyperPlaneCoefficients.traverse) (inRange 100)
                 pure $ MyPlane h
  shrink (MyPlane p) = MyPlane <$> shrink p

-- instance (Arbitrary a, Ord a) => Arbitrary (NESet.NESet a) where
--   arbitrary = do x  <- arbitrary
--                  xs <- arbitrary
--                  pure $ NESet.fromList (x :| xs)


inRange m x = abs x <= m

randomizedSameAsBruteForce :: Spec
randomizedSameAsBruteForce = describe "randomized lower envelope tests" $ do
    prop "randomized should be the same as brute force" $
      \(hs :: NESet.NESet MyPlane) ->
        Randomized.computeVertexForm (mkStdGen 1) hs `shouldBe` BruteForce.computeVertexForm hs

    prop "definers start with up direction (brute force)" $
      \(hs :: NESet.NESet MyPlane) ->
        verifyStartWithUp $ BruteForce.computeVertexForm hs

    -- prop "definers start with up direction (randomized)" $
    --   \(hs :: NESet.NESet MyPlane) ->
    --     verifyStartWithUp $ Randomized.computeVertexForm (mkStdGen 1) hs


subsets    :: Foldable f => f a -> [NonEmpty a]
subsets xs = mapMaybe NonEmpty.nonEmpty
           . filter (\ys -> length ys >= 6)
           . List.sortBy (comparing length) $ subsets' $ toList xs

subsets' []     = [[]]
subsets' (x:xs) = concatMap (\r -> [r,x:r]) (subsets' xs)

-- smallest hs = toList $ List.head $ filter isBug $ subsets hs
--   where
-- s    isBug hsL
--       | traceShow ("===========", hsL, "=================") False  = undefined
--       | otherwise = let hs' = NESet.fromList hsL in
--         Randomized.computeVertexForm (mkStdGen 1) hs' /= BruteForce.computeVertexForm hs'

-- debug1 = smallest $ NonEmpty.fromList
--                  [NonVerticalHyperPlane $ fromList' [-25.8,-25.5,-0.94445]
--                  ,NonVerticalHyperPlane $ fromList' [-20.25,-16.66667,-28]
--                  ,NonVerticalHyperPlane $ fromList' [-19,-18.88889,10]
--                  ,NonVerticalHyperPlane $ fromList' [-17.83334,1.47058,14.18181]
--                  ,NonVerticalHyperPlane $ fromList' [-15.8,-6.64706,-25.66667]
--                  ,NonVerticalHyperPlane $ fromList' [-15.46667,-11.95239,-0.8]
--                  ,NonVerticalHyperPlane $ fromList' [-15.44445,-27.66667,28]
--                  ,NonVerticalHyperPlane $ fromList' [-15.06667,-25.75,8.73076]
--                  ,NonVerticalHyperPlane $ fromList' [-15,9.66666,-18]
--                  ]
--   where
--     fromList' [a,b,c] = Vector3 a b c


buggyPlanes =  NonEmpty.fromList
                 -- [NonVerticalHyperPlane $fromList' [-25.8,-25.5,-0.94445]
                 -- ,NonVerticalHyperPlane $fromList' [-20.25,-16.66667,-28.0]
                 -- ,NonVerticalHyperPlane $fromList' [-19.0,-18.88889,10.0]
                 -- ,NonVerticalHyperPlane $fromList' [-17.83334,1.47058,14.18181]
                 -- ,NonVerticalHyperPlane $fromList' [-15.8,-6.64706,-25.66667]
                 -- ,NonVerticalHyperPlane $fromList' [-15.46667,-11.95239,-0.8]

                 [NonVerticalHyperPlane $ fromList' [-25.8,-25.5,-0.94445]
                 ,NonVerticalHyperPlane $ fromList' [-20.25,-16.66667,-28]
                 ,NonVerticalHyperPlane $ fromList' [-19,-18.88889,10]
                 ,NonVerticalHyperPlane $ fromList' [-17.83334,1.47058,14.18181]
                 ,NonVerticalHyperPlane $ fromList' [-15.8,-6.64706,-25.66667]
                 ,NonVerticalHyperPlane $ fromList' [-15.46667,-11.95239,-0.8]
                 ,NonVerticalHyperPlane $ fromList' [-15.44445,-27.66667,28]
                 ,NonVerticalHyperPlane $ fromList' [-15.06667,-25.75,8.73076]
                 ,NonVerticalHyperPlane $ fromList' [-15,9.66666,-18]
                 ]

fromList' [a,b,c] = Vector3 a b c
fromList' _ = error "fromList'"

debug = do let hs :: NonEmpty (Plane R)
               hs = buggyPlanes
                 -- ,NonVerticalHyperPlane $ fromList' [0,0,1]


                 -- ,NonVerticalHyperPlane $ fromList' [-14,-7.1,27.90909]


                 -- ,NonVerticalHyperPlane $ fromList' [-10.56522,-15.45455,-15.47827]
                 -- ,NonVerticalHyperPlane $ fromList' [-9,12.5,-26.06667]
                 -- ,NonVerticalHyperPlane $ fromList' [-6.75,-7.52174,-17]



                 -- ,NonVerticalHyperPlane $ fromList' [-5,17.0909,-24.85]
                 -- ,NonVerticalHyperPlane $ fromList' [-1.5,14.33333,-18.8]
                 -- ,NonVerticalHyperPlane $ fromList' [-0.92,-20.27273,-16.65385]




                 -- ,NonVerticalHyperPlane $ fromList' [1.22222,-18.66667,-2.83334]
                 -- ,NonVerticalHyperPlane $ fromList' [2.66666,-28,-5.13334]
                 -- ,NonVerticalHyperPlane $ fromList' [7.17647,-22.78572,21.64285]
                 -- ,NonVerticalHyperPlane $ fromList' [9.4,-23,-0.38462]
                 -- ,NonVerticalHyperPlane $ fromList' [10.20833,24.53846,27]
                 -- ,NonVerticalHyperPlane $ fromList' [10.47368,-3.68422,9.83333]
                 -- ,NonVerticalHyperPlane $ fromList' [23.28571,-13.54546,-1.375]
                 -- ,NonVerticalHyperPlane $ fromList' [24.17857,-16.39286,1.08333]
                 -- ,NonVerticalHyperPlane $ fromList' [24.66666,26.125,25.75]
                 -- ]

                 -- [NonVerticalHyperPlane $ fromList' [-8,6,-1.8]
                 -- ,NonVerticalHyperPlane $ fromList' [-6,-7.66667,5]
                 -- ,NonVerticalHyperPlane $ fromList' [-5.83334,6.75,1.4]
                 -- ,NonVerticalHyperPlane $ fromList' [3,6,1.33333]
                 -- ,NonVerticalHyperPlane $ fromList' [4.2,-5.33334,0.66666]
                 -- ,NonVerticalHyperPlane $ fromList' [7.8,4,-5]
                 -- ]

               -- hs = NonEmpty.fromList
               --      [NonVerticalHyperPlane $ fromList'  [-5,-3.33334,0.83333]
               --      ,NonVerticalHyperPlane $ fromList' [-5,2.33333,-3]
               --      ,NonVerticalHyperPlane $ fromList' [-3.83334,-4.66667,5.5]
               --      ,NonVerticalHyperPlane $ fromList' [-3.66667,0.25,3]
               --      ,NonVerticalHyperPlane $ fromList' [0,-6,-1]
               --      ,NonVerticalHyperPlane $ fromList' [0,-6,5.25]
               --      ,NonVerticalHyperPlane $ fromList' [0,4.33333,4.66666]
               --      ]

                 -- [ NonVerticalHyperPlane $ fromList' [-7.2,7.5,0.5]
                    -- , NonVerticalHyperPlane $ fromList' [-5.28572,5,-8]
                    -- , NonVerticalHyperPlane $ fromList' [-2,4.75,-5.83334]
                    -- , NonVerticalHyperPlane $ fromList' [0.25,-6.625,2]
                    -- , NonVerticalHyperPlane $ fromList' [3.57142,-6.2,1.71428]
                    -- , NonVerticalHyperPlane $ fromList' [4,7.625,3.66666]
                    -- , NonVerticalHyperPlane $ fromList' [8,-6,-0.28572]
                    -- , NonVerticalHyperPlane $ fromList' [8,5.66666,-2.125]
                    -- , NonVerticalHyperPlane $ fromList' [8,7.75,4.66666]
                    -- ]

               v  = Point3 (-0.24717) (-3.55263) 0
           for_ hs $ \h ->
             print $ (h, evalAt (projectPoint v) h
                     ,verticalSideTest v h
                     )

           env1 <- renderToIpe [osp|/tmp/bruteforce.ipe|] BruteForce.computeVertexForm hs
           env2 <- renderToIpe [osp|/tmp/randomized.ipe|] (Randomized.computeVertexForm (mkStdGen 1)) hs
           print $ env1 == env2






-- buggySideTest = it "buggy sideTest" $
--                   verticalSideTest v h `shouldBe` GT
--   where
--     v :: Point 3 R
--     v  = Point3 4.18748 7.16404 (-43.98427)
--     h = ,
--                      )


verifyStartWithUp env =  let startWithUp        :: Point 3 R -> Definers MyPlane -> All
                             startWithUp p defs = let h = NonEmpty.head $ toNonEmpty defs
                                                      q = projectPoint p .+^ Vector2 0 1
                                                      z = evalAt q h
                                                  in All $ all (\h' -> evalAt q h' >= z) defs
                                    -- verify that h is the lowest at q
                         in getAll $ Map.foldMapWithKey startWithUp env


renderToIpe             :: (Plane_ plane R, Ord plane, Show plane
                           ) => OsPath -> _ -> NonEmpty plane -> IO _
renderToIpe fp mkEnv hs =
   do writeIpeFile fp . addStyleSheet opacitiesStyle $ singlePageFromContent out
      pure $ env
  where
    Just env = connectedLowerEnvelopeWith mkEnv hs

    out :: [IpeObject R]
    out = zipWith render (cycle $ drop 3 basicNamedColors)
        . toList . NEMap.assocs . HGeometry.Plane.LowerEnvelope.asMap $ env

    -- render :: IpeColor R -> (Point 3 R, Definers plane) -> IpeObject R
    -- render color (p, _defs) = iO $ defIO (projectPoint @2 p) ! attr SStroke color

    render color (h, region) = iO' $ ipeGroup
                               [ iO $ labelled centroid' defIO' (region :+ h)
                               -- , iO $ ipeLabel (c :+ h)
                               -- , iO $ defIO (site^.asPoint) ! attr SStroke  color
                               -- ,
                               ]
      where
        defIO' reg = defIO reg ! attr SFill    color
                               ! attr SOpacity (Text.pack "10%")
        centroid' reg = centerPoint (boundingBox $ toPoly reg)
        toPoly reg = case toConvexPolygonIn rect' region of
            Left pg  -> (pg&vertices %~ view asPoint :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 R))
            Right pg -> pg&vertices %~ view asPoint

          where
            rect' = grow 1000 $ boundingBox region
