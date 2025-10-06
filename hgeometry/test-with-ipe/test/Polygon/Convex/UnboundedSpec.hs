{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polygon.Convex.UnboundedSpec where

import Control.Lens
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable
import Data.Maybe
import Golden
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Kernel
import HGeometry.HalfLine
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Instances ()
import HGeometry.Polygon.Convex.Unbounded
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec.WithTempFile

import Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 10


-- data OneOrTwo a =

toList' = \case
  Left x   -> [x]
  Right xs -> toList xs

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Polygon.Convex.Unbounded" $ do
         prop "unbounded extremal vertices in intersection" $
            \(region :: UnboundedConvexRegion (Point 2 R)) ->
              foldMap Every
                [ counterexample (show (v,h)) $
                    v `intersects` h

                | h <- toList (unboundedBoundingHalfplanes region)
                , v <- toList' (extremalVertices region)
                ]

         -- unboundeds <- runIO $ sample' $ arbitrary @(UnboundedConvexRegion (Point 2 R))
         let unboundeds = [ub]
         let draw region = foldMap (\h -> [iO $ ipeHalfPlane red h])
                                   (unboundedBoundingHalfplanes region)
                           <>
                             [iO $ defIO region]

         goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
               (ipeFileGolden { name = [osp|unboundedBoundingHalfplanes|]
                              }
               )
               ( let content' = foldMap draw $ take 1 $ reverse unboundeds
                 in addStyleSheet opacitiesStyle $ singlePageFromContent content'
               )


         goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
               (ipeFileGolden { name = [osp|unboundedIntersection|]
                              }
               )
              ( let content' = [ iO $ defIO ub1
                               , iO $ defIO line1
                               ]
                 in addStyleSheet opacitiesStyle $ singlePageFromContent content'
               )

         -- runIO $ print (tri2 `intersect`  ub4)
         -- runIO $ print (tri2 `intersects` ub4)

         prop "bug" $
           (tri2 `intersects` ub4) === isJust (tri2 `intersect` ub4)

         prop "bug vertices 1" $
            anyOf (vertices.asPoint) (`intersects` tri2) ub4 === False
         prop "bug vertices 2" $
           anyOf (vertices.asPoint) (`intersects` ub4) tri2  === False

         prop "bug tri boundary " $
           anyOf outerBoundaryEdgeSegments (`intersects` ub4) tri2 === False

         it "bug line x ub4 should be " $
           (linex `intersect` ub4) `shouldSatisfy` \case
             Just (Line_x_UnboundedConvexRegion_HalfLine _) -> True
             _ -> False

         let [a,b,c] = tri2^..outerBoundaryEdgeSegments
         it ("a intersects reg " <> show a) $
            (a `intersects` ub4) `shouldBe` False
         it "b intersects reg" $
            (b `intersects` ub4) `shouldBe` False
         it "c intersects reg" $
            (c `intersects` ub4) `shouldBe` False

         prop "the halfline that is the intersection should not intersect a" $
           case supportingLine a `intersect` ub4 of
             Just (Line_x_UnboundedConvexRegion_HalfLine hl) ->
               counterexample (show a) .
                 counterexample (show $ supportingLine a) .
                   counterexample (show hl) $
                    counterexample (show $ hl `intersect` a) $
                     not (hl `intersects` a)
             --   HalfLine (Point2 (-13.5037) (-9.47631)) (Vector2 (-5) (-1.75))
             _ -> property False

         it "bug edges x ub4 should be " $
           (linex `intersect` ub4) `shouldSatisfy` \case
             Just (Line_x_UnboundedConvexRegion_HalfLine _) -> True
             _ -> False


         -- runIO $ traverse_ print $
         --   tri2^..outerBoundaryEdgeSegments.to (\seg -> (seg, seg `intersects` ub4
         --                                                , supportingLine seg `intersect` ub4
         --                                                ))

         prop "bug : triangle intersects unbounded polygon is constent with intersect" $
           let tri = tri3 ; convex = ub5 in
            counterexample (show $ tri `intersect` convex) $
              tri `intersects` convex === isJust (tri `intersect` convex)

         let [a',b',c'] = tri3^..outerBoundaryEdgeSegments
         it ("a' intersects reg " <> show a) $
            (a' `intersects` ub5) `shouldBe` False
         it "b' intersects reg" $
            (b' `intersects` ub5) `shouldBe` False
         it "c' intersects reg" $
           counterexample (show c') $ counterexample (show $ supportingLine c') $
            counterexample (show $ (supportingLine c') `intersect` ub5) $
             (c' `intersects` ub5) `shouldBe` False

         modifyMaxDiscardRatio (*4) $
           prop "line x unbounded region intersects in the correct halfline" $
             \(line :: LinePV 2 R) (region :: UnboundedConvexRegion (Point 2 R)) ->
                case line `intersect` region of
                  Just (Line_x_UnboundedConvexRegion_HalfLine hl@(HalfLine p v)) ->
                     let q = p .+^ v
                     in counterexample (show hl) $ counterexample (show q) $
                          q `intersects` region
                             -- verify that q actually lies in the region.
                  _ -> discard

         -- goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
         --       (ipeFileGolden { name = [osp|unboundedIntersectionTriangle|]
         --                      }
         --       )
         --      ( let content' = [ iO $ defIO tri3
         --                       , iO $ defIO ub5
         --                       ]
         --         in addStyleSheet opacitiesStyle $ singlePageFromContent content'
         --       )


linex :: LinePV 2 R
linex = let p = Point2 5 (-3)
         in LinePV p (Point2 0 (-4.75) .-. p)
-- tri1 :: Triangle (Point 2 R)
-- tri1 = Triangle (Point2 8 8.1875) (Point2 (-9) (-7)) (Point2 12 15.22222)

-- ub3 :: UnboundedConvexRegion (Point 2 R)
-- ub3 = Unbounded (Vector2 5.66666 0) (Point2 (-7.33334) (-6) :| [Point2 6.28571 10.5]) (Vector2 1.83929 5.16666)


ub :: UnboundedConvexRegion (Point 2 R)
ub = Unbounded (Vector2 0 (-1)) (Point2 (-2) 0 :| [Point2 (-1.5) (-1),Point2 1 0])
               (Vector2 0.5 1)


line1 :: LinePV 2 R
line1 = LinePV (Point2 6.92307 (-4)) (Vector2 (-14) (-8.42858))
ub1 :: UnboundedConvexRegion (Point 2 R)
ub1 = Unbounded (Vector2 1.66667 (-3.08334)) (Point2 (-5.66667) (-7.08334) :| [Point2 (-3.28572) (-10.5)]) (Vector2 8.14286 (-2.75))


  -- (Point2 1 0,HalfSpace Positive (LinePV (Point2 (-2) 0) (Vector2 0 (-1))))


tri2 :: Triangle (Point 2 R)
tri2 = Triangle (Point2 5 (-3)) (Point2 0 (-4.75)) (Point2 0 0)

ub4 :: UnboundedConvexRegion (Point 2 R)
ub4=  Unbounded (Vector2 10.25 8.6) (Point2 3.75 5 :| []) (Vector2 (-6.5) 1.66666)


tri3 :: Triangle (Point 2 R)
tri3 = Triangle (Point2 0 (-1)) (Point2 0 0) (Point2 2 0)

ub5 :: UnboundedConvexRegion (Point 2 R)
ub5 = Unbounded (Vector2 0.5 (-1.83334)) (Point2 0 0.66666 :| [Point2 1.33333 1]) (Vector2 1.66667 0.66666)
