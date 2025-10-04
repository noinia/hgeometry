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

dataPath :: OsPath
dataPath = [osp|data/test-with-ipe/Triangle/|]

ipeSpec            :: OsPath -> OsPath -> Spec
ipeSpec inFp outFP = do input <- runIO go
                        ipeSpec' outFP input
  where
    go = do [chain :+ _] <- readAllFrom $ dataPath <> inFp
            [tri   :+ _] <- readAllFrom $ dataPath <> inFp
            pure (chain, tri)

ipeSpec'                   :: OsPath
                           -> ( UnboundedConvexRegionF R NonEmpty (Point 2 R)
                              , Triangle (Point 2 R)
                              )
                           -> Spec
ipeSpec' outFP (chain,tri) =
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
--     , iO $ defIO ((toBoundedFrom tri chain)&vertices %~ (^.asPoint)  :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 R)
--                 ) ! attr SFill blue
    ]







    -- -- we find a halfplane that contains the cone; so just use one of the vectors as the bounding line.d

    -- v' = quadrance w *^ v
    -- w' = quadrance v *^ w

    -- bisec = traceShow (quadrance v', quadrance w') $
    --   w' ^+^ negated v' -- note that v is pointing in the wrong dir.
    -- u     = rot90 bisec -- perpendicular to bisec
    -- h     = HalfSpace Positive (fromPointAndVec p u :: LinePV 2 R)
    -- rot90 (Vector2 x y) = Vector2 (-y) x -- rotates clockwise 90 degrees


spec :: Spec
spec = describe "triangle x unbounded convex polygon intersection" $ do
         it "manual test" $
           (myTriangle `intersects` upperQuadrant) `shouldBe` True
         it "manual test" $
           (touchingTriangle `intersects` upperQuadrant) `shouldBe` True
         it "outside test" $
           (outsideTriangle `intersects` upperQuadrant) `shouldBe` False
         ipeSpec' [osp|bug_out|] ( unboundedPoly
                                 , Triangle origin (Point2 1000 0) (Point2 0 500)
                                 )
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



unboundedPoly :: UnboundedConvexRegion (Point 2 R)
unboundedPoly = Unbounded (Vector2 1 1.55555)
                          (NonEmpty.fromList [(Point2 337.54545 636.18181)])
                          (Vector2 (-1) 18)
