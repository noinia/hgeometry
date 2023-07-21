module Polygon.Convex.ConvexSpec
  (spec
  ) where

import           HGeometry.ConvexHull.GrahamScan (convexHull)
import           Control.Lens
import           HGeometry.Ext
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Random
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Class
import           HGeometry.Transformation
import           System.Random.Stateful
import           Control.Monad.State
import           HGeometry.Point
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Test.QuickCheck ( Arbitrary(..) , sized , suchThat, choose )
import           Test.QuickCheck.Instances ()
import           Test.Hspec.QuickCheck
import           Data.Default.Class
import           Hiraffe.Graph
import           HGeometry.Instances ()

--------------------------------------------------------------------------------

-- type R = RealNumber 10

instance Default (Point 2 Rational) where
  def = origin
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
          minkowskiSum p (centerAtOrigin q) == naiveMinkowski p (centerAtOrigin q)
--   specify "∀ convex. verifyConvex convex == True" $
--     property $ \(convex :: ConvexPolygon () Rational) ->
--       verifyConvex convex
--   specify "∀ convex. extremes convex == extremesLinear convex" $
--     property $ \(convex :: ConvexPolygon () Rational, u :: Vector 2 Rational) ->
--       quadrance u > 0 ==>
--       let fastMax = snd (extremes u convex)
--           slowMax = snd (extremesLinear u (convex^.simplePolygon))
--       in cmpInDirection u fastMax slowMax === EQ
--   specify "∀ poly. extremes (convexHull poly) == extremesLinear poly" $
--     property $ \(p :: SimplePolygon () Rational, u :: Vector 2 Rational) ->
--       quadrance u > 0 ==>
--       let hull = over simplePolygon toCounterClockWiseOrder $
--             convexHull (CV.toNonEmpty (p^.outerBoundaryVector))
--           fastMax = snd (extremes u hull)
--           slowMax = snd (extremesLinear u p)
--       in cmpInDirection u fastMax slowMax === EQ

--   -- Check that vertices are always considered to be OnBoundary.
--   specify "inConvex boundary convex == OnBoundary" $
--     property $ \(convex :: ConvexPolygon () Rational) ->
--       let s = convex^.simplePolygon in
--       forAll (choose (0, size s-1)) $ \n ->
--         inConvex (s^.outerVertex n.core) convex === OnBoundary

--   -- Check that all edge points are considered to be OnBoundary.
--   specify "inConvex edge_point convex == OnBoundary" $
--     property $ \(convex :: ConvexPolygon () Rational, ZeroToOne r) ->
--       let s = convex^.simplePolygon in
--       forAll (elements (listEdges s)) $ \(LineSegment' a b) ->
--         let pt = Point $ lerp r (coerce $ a^.core) (coerce $ b^.core) in
--         inConvex pt convex === OnBoundary

--   -- Check that inConvex matches inPolygon inside the bounding box.
--   specify "inConvex pt convex == inPolygon pt convex" $
--     property $ \(convex :: ConvexPolygon () Rational) ->
--       let s = convex^.simplePolygon in
--       let bb = boundingBox convex in
--       forAll (arbitraryPointInBoundingBox bb) $ \pt ->
--         inConvex pt convex === inPolygon pt s

--   -- Points that lie on straight lines between vertices are a corner case.
--   -- Make sure that they work as expected.
--   specify "inConvex inner edge convex == inPolygon pt convex" $
--     property $ \(convex :: ConvexPolygon () Rational) ->
--       let s = convex^.simplePolygon in
--       forAll (choose (0, size s-1)) $ \a ->
--       forAll (choose (0, size s-1)) $ \b ->
--       size s > 3 && abs (a-b) >= 2 && abs (a-b) /= size s-1 ==>
--       let aPt = s ^. outerVertex a.core
--           bPt = s ^. outerVertex b.core
--           cPt = Point $ lerp 0.5 (coerce aPt) (coerce bPt)
--       in inConvex cPt convex === Inside

--   -- Verify that convexPolygon always returns convex polygons.
--   specify "verifyConvex (convexPolygon p)" $
--     property $ \(p :: SimplePolygon () R) ->
--       verifyConvex (convexPolygon p) .&&.
--       isSimple (convexPolygon p ^. simplePolygon)

--   specify "convexPolygon p `superset` p" $
--     property $ \(p :: SimplePolygon () R) ->
--       forAll (choose (0, size p-1)) $ \n ->
--         inConvex (p^.outerVertex n.core) (convexPolygon p) =/= Outside

--   specify "convexPolygon convex == convex" $
--     property $ \(p :: ConvexPolygon () Rational) ->
--       size (convexPolygon (p^.simplePolygon)^.simplePolygon)
--       ===
--       size (p^.simplePolygon)

--   specify "area (convexPolygon p) >= area p" $
--     property $ \(p :: SimplePolygon () R) ->
--       area (convexPolygon p ^. simplePolygon) >= area p

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
centerAtOrigin    :: ( SimplePolygon_ polygon  point r
                     , Fractional r
                     , IsTransformable polygon
                     ) => polygon -> polygon
centerAtOrigin pg = translateBy (origin .-. centroid pg) pg


naiveMinkowski     :: ( Ord r, Num r
                      , ConvexPolygon_ convexPolygon  point r
                      , ConvexPolygon_ convexPolygon' point' r
                      , Default point'
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
