{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.SPTSpec(spec) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Bifoldable
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.DualTree
import           HGeometry.Polygon.Simple.ShortestPath.Tree
import           HGeometry.Polygon.Triangulation
import           HGeometry.Trie
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
--------------------------------------------------------------------------------

-- type R = RealNumber 5

spec :: Spec
spec = describe "shortest path tree tests" $ do
         it "inConeTest" $
           let a, l, r, w :: Point 2 R
               a = Point2 81.766 130.9508
               l = Point2 80 160
               r = Point2 96 112
               w = Point2 128 80
           in inApexCone w a l r `shouldBe` InCone

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
        tree' = orientDualTree $ toTreeRep triang mySource tree

        mkEdge u = \case
          Left s        -> ClosedLineSegment u s
          Right (p :+ _) -> ClosedLineSegment u p

        sptEdges = [ iO $ defIO (mkEdge v p) ! attr SStroke green
                   | (v :+ _) :+ p <- computeShortestPaths' mySource triang
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
              , iO $ ipeGroup sptEdges
              , drawDualTree triang tree
              ]

    runIO $ print tree'




    -- it "dual tree correct orientations" $
    --   incorrectDualTreeNodes triang tree `shouldBe` []
    -- disabled this test for now; since the implemnentation of orientDualTree
    -- is almost verbatim taken from the test below

    it "dual tree correct edge orientations" $
      inCorrectOrientations tree' `shouldBe` []


    goldenWith [osp|data/test-with-ipe/Polygon/Simple/ShortestPath/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)



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
