{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polygon.Convex.UnboundedSpec
  (spec
  ) where


import Control.Lens
-- import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable
import Golden
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Line
import HGeometry.Polygon.Convex.Instances ()
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec.WithTempFile

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
                [ counterexample (show (v,h)) $ v `intersects` h
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

         runIO $ print (line1 `intersect` ub1)

         runIO $ print (line1 `intersects` ub1)
         runIO $ print (boundedCore ub1)
         runIO $ print (line1 `intersects` boundedCore ub1)

         goldenWith [osp|data/test-with-ipe/golden/Polygon/Convex|]
               (ipeFileGolden { name = [osp|unboundedIntersection|]
                              }
               )
              ( let content' = [ iO $ defIO ub1
                               , iO $ defIO line1
                               ]
                 in addStyleSheet opacitiesStyle $ singlePageFromContent content'
               )



ub :: UnboundedConvexRegion (Point 2 R)
ub = Unbounded (Vector2 0 (-1)) (Point2 (-2) 0 :| [Point2 (-1.5) (-1),Point2 1 0])
               (Vector2 0.5 1)


line1 :: LinePV 2 R
line1 = LinePV (Point2 6.92307 (-4)) (Vector2 (-14) (-8.42858))
ub1 :: UnboundedConvexRegion (Point 2 R)
ub1 = Unbounded (Vector2 1.66667 (-3.08334)) (Point2 (-5.66667) (-7.08334) :| [Point2 (-3.28572) (-10.5)]) (Vector2 8.14286 (-2.75))


  -- (Point2 1 0,HalfSpace Positive (LinePV (Point2 (-2) 0) (Vector2 0 (-1))))
