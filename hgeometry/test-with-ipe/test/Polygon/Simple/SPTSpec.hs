{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.SPTSpec(spec) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import           HGeometry.Polygon.Simple.ShortestPath.Tree
import           Ipe
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

-- type R = RealNumber 5

spec :: Spec
spec = describe "shortest path tree tests" $ do
         testIpe [osp|simple.ipe|]
                 [osp|simple_out.ipe|]

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (sources, poly) <-  runIO $ do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/ShortestPath/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (pts :: NonEmpty (Point 2 R :+ _))            = NonEmpty.fromList $ readAll page
            ((pg :: SimplePolygon (Point 2 R) :+ _) : _)  = readAll page
        pure (pts,pg)

    let triang    = triangulate (poly^.core)
        (mySource :+ _)  = NonEmpty.head sources
        Just tree  = dualTreeFrom mySource triang
        -- tree' = toDualTreeFrom triang mySource tree'
        out = [ iO' sources
              , iO' poly
              , drawDualTree triang tree
              ]
    goldenWith [osp|data/test-with-ipe/Polygon/Simple/ShortestPath/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)


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
    verts     = drawRoot : foldMap ((:[]) . drawVertex) dt
    treeEdges = []

    drawRoot     = drawVertex $ dt^.rootVertex
    drawVertex f = iO $ ipeDiskMark $ gr^?!interiorFacePolygonAt f.to centroid
