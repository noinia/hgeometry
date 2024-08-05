
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Plane.LowerEnvelopeSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Foldable
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Tree (Tree(..))
import           Golden
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Plane.LowerEnvelope.Connected.Separator
import           HGeometry.Plane.LowerEnvelope.Connected.Split ( findNode, pathToTree
                                                               , pathToList
                                                               , initialSplit
                                                               , initialSplitToTree
                                                               , Split(..)
                                                               , InitialSplit(..)
                                                               )
-- import qualified HGeometry.Plane.LowerEnvelope.Connected.Split as Split
import           HGeometry.Plane.LowerEnvelope.ConnectedNew
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

instance (HasDefaultIpeOut point, Point_ point 2 r, Fractional r, Ord r
         , Show point, Show r
         )
         => HasDefaultIpeOut (Region r point) where
  type DefaultIpeOut (Region r point) = Path
  defIO region = defIO $ ((uncheckedFromCCWPoints $ map (^.asPoint) vertices)
                         :: ConvexPolygon (Point 2 r)
                         )

    -- case fromPoints $ map (^.asPoint) vertices of
    --   Nothing -> error $ "could not create convex polygon?" <> show vertices
    --   Just pg -> defIO (pg :: ConvexPolygon (Point 2 r))
    where
      vertices = case region of
        Bounded vs        -> vs
        Unbounded v pts u -> let p  = NonEmpty.head pts
                                 q  = NonEmpty.last pts
                                 p' = p .-^ (1000 *^ v) -- TODO: clip this somewhere
                                 q' = q .+^ (1000 *^ u) -- TODO: clip this somewhere
                             in q' : p' : toList pts


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

instance ( Point_ point 2 r, Fractional r, Ord r

         , Show point, Show r
         )
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

         testIpeGraph [osp|foo.ipe|]
                      [osp|foo_graph_out|]


         describe "planar separator tests" $ do
           prop "findNode on path the same" $
             \(t :: Tree Int) ->
               let predicates = const False : [(== x) | x <- toList t]
                   findNodeSame t' p = case findNode p t' of
                     Nothing    -> True
                     Just path' -> pathToTree path' == t'
               in all (findNodeSame t) predicates
           prop "initialSplit identity" $
             \(t :: Tree Int) -> onAllPairs t $ \e ->
               let s = initialSplit e t
               in (treeEdges $ initialSplitToTree s) === treeEdges t
           prop "initialSplit, partition" $
             \(t0 :: Tree Int) -> let t = makeUnique t0 in onAllPairs t $ \e ->
               let allElems    = Set.fromList $ toList t
                   mkSet       = foldMap (foldMap Set.singleton)
               in case initialSplit e t of
                 DecendantSplit r before' path' after'                               ->
                   let pathElems   = Set.fromList (pathToList path')
                       beforeElems = mkSet before'
                       afterElems  = mkSet after'
                       rootElem    = Set.singleton r
                   in formPartition allElems rootElem pathElems Set.empty
                                             beforeElems Set.empty afterElems
                 InternalSplit r (Split (Vector2 lPath rPath) before' middle after') ->
                   let lPathElems  = Set.fromList $ pathToList lPath
                       rPathElems  = Set.fromList $ pathToList rPath
                       beforeElems = mkSet before'
                       middleElems = mkSet middle
                       afterElems  = mkSet after'
                       rootElem    = Set.singleton r
                   in formPartition allElems rootElem lPathElems rPathElems
                                             beforeElems middleElems afterElems

formPartition allElems rootElem lPathElems rPathElems
              beforeElems middleElems afterElems  =
  conjoin $ (allElems === Set.unions [ rootElem
                                     , lPathElems , rPathElems
                                     , beforeElems, middleElems, afterElems])
  : map pairwiseDisjoint [ (rootElem,    lPathElems)
                         , (rootElem,    rPathElems)
                         , (rootElem,    beforeElems)
                         , (rootElem,    middleElems)
                         , (rootElem,    afterElems)
                         , (lPathElems,  rPathElems)
                         , (lPathElems,  beforeElems)
                         , (lPathElems,  middleElems)
                         , (lPathElems,  afterElems)
                         , (rPathElems,  beforeElems)
                         , (rPathElems,  middleElems)
                         , (rPathElems,  afterElems)
                         , (beforeElems, middleElems)
                         , (beforeElems, afterElems)
                         , (middleElems, afterElems)
                         ]
  where
    pairwiseDisjoint (as,bs) = Set.null (Set.intersection as bs) === True


makeUnique :: Tree Int -> Tree Int
makeUnique = snd . go 0
  where
    go i (Node _ chs) = Node i <$> List.mapAccumL go (i+1) chs

onAllPairs     :: Tree Int -> ((Int,Int) -> Property) -> Property
onAllPairs t f = conjoin $ map f allPairs
  where
    allPairs = [(x,y) | x <- toList t, y <- toList t, x /= y]


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

-- | Build voronoi diagrams on the input points
testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let vd = voronoiDiagram' $ view core <$> points
        vv = voronoiVertices $ view core <$> points
        out = [ iO' points
              , iO' vd
              ] <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)

theEdges :: PlaneGraph (Point 2 R) h (E R) -> IpeObject' Group R
theEdges = ipeGroup . Map.foldMapWithKey (\v (adjs, _) ->
               foldMap (\w -> [ iO $ defIO (ClosedLineSegment v w)
                              ]) adjs)

drawSeparator                     :: Separator (Point 2 R) -> ( IpeObject' Group R
                                                              , Vector 2 (IpeObject' Group R))
drawSeparator (sep,Vector2 as bs) =
    ( draw purple sep, Vector2 (draw red as) (draw blue bs))
  where
    draw c pts = ipeGroup [ iO $ defIO p ! attr SStroke c
                                         ! attr SSize (IpeSize $ Named "large")
                          | p <- pts
                          ]


drawTree :: Tree (Point 2 R) -> IpeObject' Group R
drawTree = ipeGroup
         . map (\(p,q) -> iO $ defIO (ClosedLineSegment p q) ! attr SPen (IpePen $ Named "fat")
               )
         . Set.toList . treeEdges


-- build a triangulated graph from the points in the input file
testIpeGraph            :: OsPath -> OsPath -> Spec
testIpeGraph inFp outFp = do
    (points :: NonEmpty (Point 2 R :+ _)) <- runIO $ do
      inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
      NonEmpty.fromList <$> readAllFrom inFp'
    let vd = voronoiDiagram' $ view core <$> points
        vv = voronoiVertices $ view core <$> points
        gr = toPlaneGraph $ Map.mapKeysMonotonic liftPointToPlane vd
        n  = length gr
        s@(sep, Vector2 as bs)   = planarSeparator gr
        (sep',Vector2 as' bs') = drawSeparator s
        out = [ iO' points
              , iO' vd
              , iO' $ theEdges gr
              , iO $ sep' ! attr SLayer "sep"
              , iO $ as'  ! attr SLayer "setA"
              , iO $ bs'  ! attr SLayer "setB"
              ] <> [ iO' $ drawTree  t ! attr SLayer "trees"
                   | t <- bff gr ]
                <> [ iO'' v $ attr SStroke red | v <- Set.toAscList vv ]
    it "separator is complete" $
      (Set.fromList $ sep <> as <> bs) `shouldBe` (Map.keysSet gr)
    it ("separator is balanced " <> show outFp <> show ("sizes",n,length as, length bs, length sep)) $
      (max (length as) (length bs) <= (2*(n `div` 3))) `shouldBe` True
    it ("separator is small " <> show outFp) $
      (length sep <= 2*floor (sqrt $ fromIntegral n)) `shouldBe` True
    it "separator is a set" $
      length (Set.fromList sep) `shouldBe` (length sep)
    it "A is a set" $
      length (Set.fromList as) `shouldBe` (length as)
    it "B is a set" $
      length (Set.fromList bs) `shouldBe` (length bs)

    goldenWith [osp|data/test-with-ipe/Plane/LowerEnvelope/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)









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

-}
