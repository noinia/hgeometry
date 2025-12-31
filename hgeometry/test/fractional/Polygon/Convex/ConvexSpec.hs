module Polygon.Convex.ConvexSpec
  (spec
  ) where

import Data.Maybe (isJust)
import Control.Lens hiding (elements)
import Control.Monad.State
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Boundary
import HGeometry.ConvexHull.GrahamScan (convexHull)
import HGeometry.Cyclic
import HGeometry.Ext
import HGeometry.Instances ()
import HGeometry.Kernel.Test.Box
import HGeometry.Kernel
import HGeometry.Intersection
import HGeometry.Polygon.Class
import HGeometry.Polygon.Convex
import HGeometry.Polygon.Convex.MinkowskiSum
import HGeometry.Polygon.Convex.Random
import HGeometry.Polygon.Instances ()
import HGeometry.Polygon.Simple
import HGeometry.Transformation
import System.Random.Stateful
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ( Arbitrary(..) , sized , suchThat, choose , forAll , (===)
                       , (==>), counterexample, elements
                       )
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

-- type R = RealNumber 10

instance Arbitrary (ConvexPolygon (Point 2 Rational)) where
  arbitrary = let granularity = 1000000 in
    sized $ \n -> do
                    k      <- choose (3, max 3 n)
                    stdgen <- arbitrary
                    pure $ evalState (randomConvex k granularity) (mkStdGen stdgen)

  -- shrink convex = map convexPolygon (shrink (convex^.simplePolygon))

--------------------------------------------------------------------------------



spec :: Spec
spec = describe "Convex Polygon tests" $ do
        prop "quickcheck minkowskisum same as naive" $ \(CP p :: CP Rational) (CP q) ->
          minkowskiSum p (centerAtOrigin q) `isShiftOf` naiveMinkowski p (centerAtOrigin q)
        prop "∀ convex. verifyConvex convex == True" $
          \(convex :: ConvexPolygon (Point 2 Rational)) ->
            verifyConvex convex
        prop "∀ convex. extremes convex == extremesLinear convex" $
          \(convex :: ConvexPolygon (Point 2  Rational), u :: Vector 2 Rational) ->
            quadrance u > 0 ==>
            let fastMax = snd (extremes u convex)
                slowMax = snd (extremes u (toSimplePolygon convex))
            in cmpInDirection u fastMax slowMax === EQ
        prop "∀ poly. extremes (convexHull poly) == extremesLinear poly" $
          \(p :: SimplePolygon (Point 2 Rational), u :: Vector 2 Rational) ->
            quadrance u > 0 ==>
            let hull    = convexHull (toNonEmptyOf outerBoundary p)
                fastMax = snd (extremes u hull)
                slowMax = snd (extremes u p)
            in cmpInDirection2 u fastMax slowMax === EQ

        -- Check that vertices are always considered to be OnBoundary.
        prop "inConvex boundary convex == OnBoundary" $
          \(convex :: ConvexPolygon (Point 2 Rational)) ->
            let n = numVertices convex
            in forAll (choose (0, n-1)) $ \i ->
              let v = (convex^.outerBoundaryVertexAt i) in
              case inPolygon v convex of
                OnBoundaryEdge j -> counterexample (show $ (j,v)) $
                                    v `onSegment` (convex^.outerBoundaryEdgeSegmentAt j)
                res              -> counterexample (show res) False

        -- Check that all edge points are considered to be OnBoundary.
        prop "inConvex edge_point convex == OnBoundary" $
          \(convex :: ConvexPolygon (Point 2 Rational), ZeroToOne r) ->
            forAll (elements $ itoListOf outerBoundaryEdgeSegments convex) $ \(i,e) ->
              let pt = interpolate r e
              in case inPolygon pt convex of
                   OnBoundaryEdge j -> counterexample (show (i,j,pt)) $
                                       pt `onSegment` (convex^.outerBoundaryEdgeSegmentAt j)
                   res              -> counterexample (show res) False

        -- Check that inConvex matches inPolygon inside the bounding box.
        prop "inConvex pt convex == inPolygon pt convex" $
          \(convex :: ConvexPolygon (Point 2  Rational)) ->
            let s = toSimplePolygon convex in
            let bb = boundingBox convex in
            forAll (arbitraryPointInBoundingBox bb) $ \pt ->
              inPolygon pt convex === inPolygon pt s

        -- Points that lie on straight lines between vertices are a corner case.
        -- Make sure that they work as expected.
        prop "inConvex inner edge convex == inPolygon pt convex" $
          \(convex :: ConvexPolygon (Point 2 Rational)) ->
              let s = toSimplePolygon convex in
              forAll (choose (0, numVertices s-1)) $ \a ->
              forAll (choose (0, numVertices s-1)) $ \b ->
              numVertices s > 3 && abs (a-b) >= 2 && abs (a-b) /= numVertices s-1 ==>
              let aPt = s ^. outerBoundaryVertexAt a
                  bPt = s ^. outerBoundaryVertexAt b
                  cPt = aPt .+^ (0.5 *^ (bPt .-. aPt))
              in inPolygon cPt convex === StrictlyInside

        -- Check that bounding box is correct
        prop "boundingBox convex == boundingBox (toSimplePolygon convex)" $
          \(convex :: ConvexPolygon (Point 2  Rational)) ->
            let s = toSimplePolygon convex in
              boundingBox convex === boundingBox s

        prop "line intersects convex polygon is constent" $
          \(line :: LinePV 2 Rational) (convex :: ConvexPolygon (Point 2 Rational)) ->
            line `intersects` convex === isJust (line `intersect` convex)

        prop "line intersects convex polygon is constent with simplePolygon" $
          \(line :: LinePV 2 Rational) (convex :: ConvexPolygon (Point 2 Rational)) ->
            line `intersects` convex === line `intersects` (toSimplePolygon convex)

        prop "line intersects convex polygon is constent with edge intersection" $
          \(line :: LinePV 2 Rational) (convex :: ConvexPolygon (Point 2 Rational)) ->
            line `intersects` convex ===
                anyOf outerBoundaryEdgeSegments (line `intersects`) convex

--   -- Verify that convexPolygon always returns convex polygons.
--   specify "verifyConvex (convexPolygon p)" $
--     property $ \(p :: SimplePolygon () R) ->
--       verifyConvex (convexPolygon p) .&&.
--       isSimple (convexPolygon p ^. simplePolygon)

--   specify "convexPolygon p `superset` p" $
--     property $ \(p :: SimplePolygon () R) ->
--       forAll (choose (0, size p-1)) $ \n ->
--         inConvex (p^.outerVertex n.core) (convexPolygon p) =/= Outside

--   specify "size (convexPolygon p) <= size p" $
--     property $ \(p :: SimplePolygon () R) ->
--       size (convexPolygon p ^. simplePolygon) <= size p

--   -- Check that Convex.diameter gives the same result as Naive.diameter
--   specify "Convex.diameter == Naive.diameter" $
--     property $ \(convex :: ConvexPolygon () Rational) ->
--       let (fastA, fastB) = diametralPair convex
--           Just (slowA, slowB) = Naive.diametralPair (toPoints (convex^.simplePolygon))
--       in
--         squaredEuclideanDist (fastA^.core) (fastB^.core) ===
--         squaredEuclideanDist (slowA^.core) (slowB^.core)


--------------------------------------------------------------------------------

-- | Center the given polygon at the origin. I.e. places the centroid at the origin.
centerAtOrigin    :: forall polygon point r.
                     ( SimplePolygon_ polygon  point r
                     , Fractional r
                     , IsTransformable polygon
                     ) => polygon -> polygon
centerAtOrigin pg = translateBy (origin .-. (centroid pg :: Point 2 r)) pg


naiveMinkowski     :: ( Ord r, Num r
                      , ConvexPolygon_ convexPolygon  point r
                      , ConvexPolygon_ convexPolygon' point' r
                      )
                   => convexPolygon -> convexPolygon'
                   -> ConvexPolygon (point :+ point')
naiveMinkowski p q = convexHull . NonEmpty.fromList
                   $ [ v .+. w | v <- p^..outerBoundary
                               , w <- q^..outerBoundary
                     ]
  where
    v .+. w = v .+^ (w^.vector) :+ w

newtype CP r = CP (ConvexPolygon (Point 2 r)) deriving (Eq,Show)

instance (Arbitrary r, Fractional r, Ord r) => Arbitrary (CP r) where
  arbitrary =  CP <$> suchThat ((\p q ps -> convexHull (p NonEmpty.:| q:ps))
                                <$> arbitrary <*> arbitrary <*> arbitrary)
                               (\p -> numVertices p > 2)
