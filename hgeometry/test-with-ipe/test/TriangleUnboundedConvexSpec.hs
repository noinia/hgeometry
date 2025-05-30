{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module TriangleUnboundedConvexSpec where

import           Control.Lens as Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Golden
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5






ipeSpec inFp outFP = do (chain,tri) <- runIO go
                        goldenWith dataPath
                                   (ipeContentGolden { name = outFP })
                                   [ case tri `intersect` chain of
                                       Nothing    -> iO $ ipeLabel ("no intersection" :+ origin)
                                                        ! attr SStroke black
                                       Just inter -> case inter of
                                         DegenerateVertex v -> iO $ defIO (v^.asPoint)
                                                                  ! attr SStroke red
                                         DegenerateEdge e   -> iO $ defIO ((^.asPoint) <$> e)
                                                                  ! attr SStroke red
                                         ActualPolygon poly -> iO $ defIO ((^.asPoint) <$> poly)
                                                                  ! attr SFill red
                                   , iO' chain
                                   , iO' tri
                                   ]
  where
    dataPath = [osp|data/test-with-ipe/Triangle/|]
    go = do [chain :+ _] <- readAllFrom $ dataPath <> inFp
            [tri   :+ _] <- readAllFrom $ dataPath <> inFp
            pure ( chain :: UnboundedConvexRegionF R NonEmpty (Point 2 R)
                 , tri   :: Triangle (Point 2 R)
                 )

spec :: Spec
spec = describe "triangle x unbounded convex polygon intersection" $ do
         it "manual test" $
           (myTriangle `intersects` upperQuadrant) `shouldBe` True
         it "manual test" $
           (touchingTriangle `intersects` upperQuadrant) `shouldBe` True
         it "outside test" $
           (outsideTriangle `intersects` upperQuadrant) `shouldBe` False
         ipeSpec [osp|triangle_x_unbounded.ipe|]
                 [osp|triangle_x_unbounded.out|]
         ipeSpec [osp|triangle_x_unbounded1.ipe|]
                 [osp|triangle_x_unbounded1.out|]
         ipeSpec [osp|triangle_x_unbounded_contained.ipe|]
                 [osp|triangle_x_unbounded_contained.out|]
         ipeSpec [osp|triangle_x_cone.ipe|]
                 [osp|triangle_x_cone.out|]
         ipeSpec [osp|triangle_x_cone_no.ipe|]
                 [osp|triangle_x_cone_no.out|]

myTriangle :: Triangle (Point 2 R)
myTriangle = Triangle (Point2 1 1) (Point2 100 0) (Point2 0 100)

outsideTriangle :: Triangle (Point 2 R)
outsideTriangle = Triangle (Point2 (-1) (-10)) (Point2 (-100) (-4)) (Point2 (-1) (-100))

touchingTriangle :: Triangle (Point 2 R)
touchingTriangle = Triangle (Point2 (-1) (-1)) (Point2 100 0) (Point2 0 (-1100))


upperQuadrant :: UnboundedConvexRegion (Point 2 R)
upperQuadrant = Unbounded (Vector2 0 (-1)) (origin :| []) (Vector2 1  0)
