{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Convex.IntersectTriangleSpec where

import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import qualified Data.Text as Text
import           Data.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import           Golden
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Interval.EndPoint
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Internal
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Triangle
import qualified HGeometry.Triangle as Triangle
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

import           Debug.Trace
import           Data.Functor.Classes
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
    pure (NonEmpty.fromList $ readAll page, NonEmpty.fromList $ readAll page)
    -- bimap NonEmpty.fromList NonEmpty.fromList
    -- let (tris :: NonEmpty (Triangle (Point 2 R) :+ _))      =
    --     (pgs  :: NonEmpty (ConvexPolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
    -- pure (tris, pgs)

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
