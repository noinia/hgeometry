{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Convex.IntersectTriangleSpec where

import           Control.Lens
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Golden
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Triangle
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Convex polygon x Triangle intersection" $ do
         testIpe [osp|intersectTriangle.ipe|]
                 [osp|intersectTriangle.out|]

loadInputs      :: OsPath -> IO ( NonEmpty (Triangle      (Point 2 R) :+ _)
                                , NonEmpty (ConvexPolygon (Point 2 R) :+ _)
                                )
loadInputs inFp = do
    inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Convex/|] <> inFp)
    Right page <- readSinglePageFile inFp'
    pure ( NonEmpty.fromList $ readAll page
         , NonEmpty.fromList $ filter (\(pg :+ _) -> numVertices pg > 3)
         $ readAll page)
    -- only report the convex polygons with > 3 vertices

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (triangles, polygons) <-  runIO $ loadInputs inFp

    for_ polygons $ \(polygon :+ _) ->
      for_ triangles $ \(triangle :+ _) -> do
        it ("intersects triangle and polygon") $
          (triangle `intersects` polygon) `shouldBe` True -- TODO; fix

    let forMap'  = flip foldMap
        content' = forMap' polygons $ \polygon ->
                      forMap' triangles $ \triangle ->
                         [ iO' polygon
                         , iO' triangle
                         , iO $ ipeGroup [ renderComponent comp
                                         | comp <- maybeToList
                                                   $ (triangle^.core) `intersect` (polygon^.core)
                                         ]
                         ]
    describe "compute intersection" $
      goldenWith [osp|data/test-with-ipe/Polygon/Convex/|]
                 (ipeFileGolden { name = outFp })
                   (addStyleSheet opacitiesStyle $ singlePageFromContent content')


        -- it ("intersect triangle and convex polygon") $ do
        --    for_ (halfPlane `intersect` polygon) $ \case


renderComponent :: forall vertex f r.
                   ( Point_ vertex 2 r
                   , VertexContainer f vertex
                   , HasFromFoldable1 f
                   )
                => PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF f vertex)
                -> IpeObject r
renderComponent = \case
    DegenerateVertex v -> iO $ defIO (v^.asPoint)
                             ! attr SStroke red
    DegenerateEdge e   -> iO $ defIO (view asPoint <$> e)
                             ! attr SStroke red
    ActualPolygon poly -> iO $ ipeSimplePolygon poly
                             ! attr SFill red
