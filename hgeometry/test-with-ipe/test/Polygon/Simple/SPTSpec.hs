{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.SPTSpec(spec) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Bifoldable
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.DualTree
import           HGeometry.Polygon.Simple.Sample
import           HGeometry.Polygon.Simple.ShortestPath.Tree
import           HGeometry.Polygon.Triangulation
import           HGeometry.Properties
import           HGeometry.Trie
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           System.Random
import           System.Random.Stateful
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

import           System.IO.Unsafe (unsafePerformIO)
--------------------------------------------------------------------------------

-- type R = RealNumber 5

data PointInPoly = PointInPoly (SimplePolygon (Point 2 R)) (Point 2 R)
  deriving (Show,Eq)

instance Arbitrary PointInPoly where
  -- general idea: generate a random polygon with Double coordiantes, then generate
  -- a point inside a random triangle, and finally convert them into type R
  arbitrary = do (poly :: SimplePolygon (Point 2 Double)) <- arbitrary
                 seed <- arbitrary
                 let pureGen = mkStdGen seed
                     s       = runStateGen_ pureGen $ samplePolygon poly
                     doubleToR = realToFrac :: Double -> R
                 pure $ PointInPoly (poly&vertices %~ over coordinates doubleToR)
                                    (s&coordinates %~ doubleToR)

spec :: Spec
spec = describe "shortest path tree tests" $ do
         prop "edges contained in polygon" $
           \(PointInPoly poly s) ->
             let triang = triangulate poly
                 sptEdges  = [ mkEdge v p | (v :+ _) :+ p <- computeShortestPaths' s triang ]
             in filter (not . (`containedIn` poly)) sptEdges === []

  {-
         prop "render" $
           \(PointInPoly poly s) (i :: Int) -> unsafePerformIO $ do
             is <- encodeFS (show i)
             let triang = triangulate poly
                 sptEdges = [ mkEdge v p | (v :+ _) :+ p <- computeShortestPaths' s triang ]
                 sptEdges' = [ iO $ defIO e ! attr SStroke green | e <- sptEdges ]
                 diags = [ iO $ defIO  (triang^?!edgeSegmentAt e) ! attr SStroke gray
                         | (e, Diagonal) <- triang^..edges.withIndex
                         ]
                 out = [ iO' s
                       , iO' poly
                       , iO $ ipeGroup diags
                       , iO $ ipeGroup sptEdges'
                       ]
                 outF = [osp|/tmp/out_|] <> is <> [osp|.ipe|]
             writeIpeFile outF $ singlePageFromContent out
             pure True
-}
         -- it "inConeTest" $
         --   let a, l, r, w :: Point 2 R
         --       a = Point2 81.766 130.9508
         --       l = Point2 80 160
         --       r = Point2 96 112
         --       w = Point2 128 80
         --   in inApexCone w a l r `shouldBe` InCone

         testIpe [osp|simpler.ipe|]
                 [osp|simpler_out|]
         testIpe [osp|simpler1.ipe|]
                 [osp|simpler1_out|]
         testIpe [osp|simpler2.ipe|]
                 [osp|simpler2_out|]
         testIpe [osp|simpler3.ipe|]
                 [osp|simpler3_out|]
         testIpe [osp|simple.ipe|]
                 [osp|simple_out|]
         testIpe [osp|simple2.ipe|]
                 [osp|simple2_out|]
         testIpe [osp|simple3.ipe|]
                 [osp|simple3_out|]
         testIpe [osp|funnel.ipe|]
                 [osp|funnel_out|]
         testIpe [osp|funnel1.ipe|]
                 [osp|funnel1_out|]
         testIpe [osp|bug.ipe|]
                 [osp|bug.out|]


mkEdge u = \case
  Left s        -> ClosedLineSegment u s
  Right (p :+ _) -> ClosedLineSegment u p

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (sources, poly) <-  runIO $ do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/ShortestPath/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (pts :: NonEmpty (Point 2 R :+ _))            = NonEmpty.fromList $ readAll page
            ((pg :: SimplePolygon (Point 2 R) :+ _) : _)  = readAll page
        pure (pts,pg)

    let triang    = triangulate (poly^.core)
        (mySource :+ _)  = NonEmpty.head sources
        Just tree  = dualTreeFrom mySource triang
        tree' = orientDualTree (==) $ toTreeRep triang mySource tree

        sptEdges  = [ mkEdge v p | (v :+ _) :+ p <- computeShortestPaths' mySource triang ]
        sptEdges' = [ iO $ defIO e ! attr SStroke green
                    | e <- sptEdges
                    ]

        diags = [ iO $ defIO  (triang^?!edgeSegmentAt e) ! attr SStroke gray
                | (e, Diagonal) <- triang^..edges.withIndex
                ]

        lefts = [ iO $ defIO p ! attr SStroke blue
                | p <- bifoldMap (\(Vector2 (l :+ _) _) -> [l]) (const []) tree'
                ]

        out = [ iO' sources
              , iO' poly
              , iO $ ipeGroup lefts
              , iO $ ipeGroup diags
              , iO $ ipeGroup sptEdges'
              , drawDualTree triang tree
              ]


    -- it "dual tree correct orientations" $
    --   incorrectDualTreeNodes triang tree `shouldBe` []
    -- disabled this test for now; since the implemnentation of orientDualTree
    -- is almost verbatim taken from the test below

    it "dual tree correct edge orientations" $
      inCorrectOrientations tree' `shouldBe` []

    it "edges contained in polygon" $
      filter (not . (`containedIn` (poly^.core))) sptEdges `shouldBe` []

    goldenWith [osp|data/test-with-ipe/Polygon/Simple/ShortestPath/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)

    -- asOpen (ClosedLineSegment a b) = OpenLineSegment a b

-- | computes the faces (represented by their face Id and a list of vertices)
--  that have the "wrong" orientation.
incorrectDualTreeNodes        :: forall graph vertex dart r.
                                 (PlaneGraph_ graph vertex, Point_ vertex 2 r, Fractional r, Ord r)
                              => graph
                              -> DualTree (FaceIx graph)
                                          dart
                                          (FaceIx graph)
                              -> [(FaceIx graph, Point 2 r, String, Point 2 r, String, Point 2 r)]
incorrectDualTreeNodes triang = go0 . trimap centroid' id centroid'
  where
    centroid'    :: FaceIx graph -> Point 2 r :+ (FaceIx graph, [vertex])
    centroid' fi = let pg = triang^?!interiorFacePolygonAt fi
                   in centroid pg :+ (fi, pg^..outerBoundary.core)

    go0 = \case
      RootZero  _       -> []
      RootOne   r a     -> go r a
      RootTwo   r a b   -> go r a <> go r b
      RootThree r a b c -> go r a <> go r b <> go r c

    go p (_, tr) = case tr of
      Leaf _         -> []
      OneNode _ _    -> []
      TwoNode v l r -> let loc = case ccwCmpAroundWith (v .-. p) v (l^._2.root) (r^._2.root) of
                                   GT -> [ (v^.extra._1
                                           , v^.core, "L:", l^._2.root.core, "R:",r^._2.root.core)]
                                   _  -> []
                       in loc <> go v l <> go v r

inCorrectOrientations :: Eq ix
                      => DualTree source
                                  (Vector 2 (vertex :+ ix))
                                  (vertex :+ ix)
                      -> [(vertex :+ ix)]
inCorrectOrientations = \case
    RootZero  _       -> []
    RootOne   _ a     -> go a
    RootTwo   _ a b   -> go a <> go b
    RootThree _ a b c -> go a <> go b <> go c
  where
    go (Vector2 l _,tr) = foldWithEdgeLabels (\v -> l =.= v)
                                             (\(Vector2 l' _) v -> l' =.= v) tr

    x =.= y = if ((==) `on` (view extra)) x y then [y] else []


drawDualTree       :: ( Point_ vertex 2 r, Ord r, Fractional r
                      , PlaneGraph_ planeGraph vertex
                      )
                   => planeGraph
                   -> DualTree (FaceIx planeGraph)
                               (DartIx planeGraph)
                               (FaceIx planeGraph)
                   -> IpeObject r
drawDualTree gr dt = iO . ipeGroup . concat $ [ verts
                                              , treeEdges
                                              ]
  where
    verts     = drawRoot : foldMap ((:[]) . iO . drawVertex) dt
    treeEdges = []

    drawRoot     = iO $ drawVertex (dt^.rootVertex) ! attr SStroke red
    drawVertex f = ipeDiskMark $ gr^?!interiorFacePolygonAt f.to centroid
