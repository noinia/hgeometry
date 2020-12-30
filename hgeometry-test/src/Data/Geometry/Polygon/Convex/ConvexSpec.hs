{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Polygon.Convex.ConvexSpec (spec) where

import           Algorithms.Geometry.ConvexHull.GrahamScan (convexHull)
import           Control.Arrow                             ((&&&))
import           Control.Lens                              (over, to, (^.), (^..))
import           Control.Monad.Random
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable                             as F
import           Data.Geometry
import           Data.Geometry.Boundary
import           Data.Geometry.Box                         (boundingBox)
import           Data.Geometry.BoxSpec                     (arbitraryPointInBoundingBox)
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.PolygonSpec                 ()
import qualified Data.List.NonEmpty                        as NonEmpty
import           Data.RealNumber.Rational
import qualified Data.Vector.Circular                      as CV
import           Paths_hgeometry_test
import           Test.Hspec
import           Test.QuickCheck                           (Arbitrary (..), choose, elements,
                                                            forAll, property, sized, suchThat,
                                                            (===), (==>))
import           Test.QuickCheck.Instances                 ()
import           Test.Util                                 (ZeroToOne (..))

--------------------------------------------------------------------------------

instance Arbitrary (ConvexPolygon () Rational) where
  arbitrary = sized $ \n -> do
    k <- choose (3, max 3 n)
    stdgen <- arbitrary
    pure $ evalRand (randomConvex k granularity) (mkStdGen stdgen)
    where
      granularity = 1000000
  shrink convex = map convexPolygon (shrink (convex^.simplePolygon))

--------------------------------------------------------------------------------

type R = RealNumber 10


spec :: Spec
spec = do
  testCases "src/Data/Geometry/Polygon/Convex/convexTests.ipe"
  specify "∀ convex. verifyConvex convex == True" $
    property $ \(convex :: ConvexPolygon () Rational) ->
      verifyConvex convex
  specify "∀ convex. extremes convex == extremesLinear convex" $
    property $ \(convex :: ConvexPolygon () Rational, u :: Vector 2 Rational) ->
      quadrance u > 0 ==>
      let fastMax = snd (extremes u convex)
          slowMax = snd (extremesLinear u (convex^.simplePolygon))
      in cmpExtreme u fastMax slowMax === EQ
  specify "∀ poly. extremes (convexHull poly) == extremesLinear poly" $
    property $ \(p :: SimplePolygon () Rational, u :: Vector 2 Rational) ->
      quadrance u > 0 ==>
      let hull = over simplePolygon toCounterClockWiseOrder $
            convexHull (CV.toNonEmpty (p^.outerBoundaryVector))
          fastMax = snd (extremes u hull)
          slowMax = snd (extremesLinear u p)
      in cmpExtreme u fastMax slowMax === EQ

  -- Check that vertices are always considered to be OnBoundary.
  specify "inConvex boundary convex == OnBoundary" $
    property $ \(convex :: ConvexPolygon () Rational) ->
      let s = convex^.simplePolygon in
      forAll (choose (0, size s-1)) $ \n ->
        inConvex (s^.outerVertex n.core) convex === OnBoundary

  -- Check that all edge points are considered to be OnBoundary.
  specify "inConvex edge_point convex == OnBoundary" $
    property $ \(convex :: ConvexPolygon () Rational, ZeroToOne r) ->
      let s = convex^.simplePolygon in
      forAll (elements (listEdges s)) $ \(LineSegment' a b) ->
        let pt = Point $ lerp r (coerce $ a^.core) (coerce $ b^.core) in
        inConvex pt convex === OnBoundary

  -- Check that inConvex matches inPolygon inside the bounding box.
  specify "inConvex pt convex == inPolygon pt convex" $
    property $ \(convex :: ConvexPolygon () Rational) ->
      let s = convex^.simplePolygon in
      let bb = boundingBox convex in
      forAll (arbitraryPointInBoundingBox bb) $ \pt ->
        inConvex pt convex === inPolygon pt s

  -- Points that lie on straight lines between vertices are a corner case.
  -- Make sure that they work as expected.
  specify "inConvex inner edge convex == inPolygon pt convex" $
    property $ \(convex :: ConvexPolygon () Rational) ->
      let s = convex^.simplePolygon in
      forAll (choose (0, size s-1)) $ \a ->
      forAll (choose (0, size s-1)) $ \b ->
      size s > 3 && abs (a-b) >= 2 && abs (a-b) /= size s-1 ==>
      let aPt = s ^. outerVertex a.core
          bPt = s ^. outerVertex b.core
          cPt = Point $ lerp 0.5 (coerce aPt) (coerce bPt)
      in inConvex cPt convex === Inside

  -- Verify that convexPolygon always returns convex polygons.
  specify "verifyConvex (convexPolygon p)" $
    property $ \(p :: SimplePolygon () Rational) ->
      verifyConvex (convexPolygon p)

  specify "area (convexPolygon p) >= area p" $
    property $ \(p :: SimplePolygon () Rational) ->
      area (convexPolygon p ^. simplePolygon) >= area p

  specify "size (convexPolygon p) <= size p" $
    property $ \(p :: SimplePolygon () Rational) ->
      size (convexPolygon p ^. simplePolygon) <= size p


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading ConvexTests file" $
                   expectationFailure . unwords $
                     [ "Failed to read ipe file", show fp, ":", show e]
    Right tcs -> do mapM_ toSpec tcs
                    minkowskiTests $ map _polygon tcs

data TestCase r = TestCase { _polygon    :: ConvexPolygon () r
                           }
                  deriving (Show)

toSingleSpec        :: (Num r, Ord r, Show r)
                    => ConvexPolygon q r -> Vector 2 r -> Expectation
toSingleSpec poly u =
  -- test that the reported extremes are equally far in direction u
    F.all allEq (unzip [extremes u poly, extremesLinear u (poly^.simplePolygon)])
    `shouldBe` True
  where
    allEq ~(p:ps) = all (\q -> cmpExtreme u p q == EQ) ps

-- | generates 360 vectors "equally" spaced/angled
directions :: Num r => [Vector 2 r]
directions = map (fmap toRat . uncurry Vector2 . (cos &&& sin) . toRad) ([0..359] :: [Double])
  where
    toRad i = i * (pi / 180)
    toRat x = fromIntegral . round $ 100000 * x

toSpec                 :: (Num r, Ord r, Show r) => TestCase r -> SpecWith ()
toSpec (TestCase poly) = do
                           it "Extreme points; binsearch same as linear" $
                             mapM_ (toSingleSpec poly) directions




readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase R])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase (ConvexPolygon poly) | (poly :+ _) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon


--------------------------------------------------------------------------------

minkowskiTests     ::  (Fractional r, Ord r, Show r) => [ConvexPolygon () r] -> Spec
minkowskiTests pgs = do
      minkowskiTests' "polygons in ipe file" pgs
      it "quickcheck minkowskisum same as naive" $
        property $ \(CP p :: CP Double) (CP q) ->
          minkowskiSum p q == naiveMinkowski p q



minkowskiTests'                      ::  (Fractional r, Ord r, Show r)
                                     => String -> [ConvexPolygon () r] -> Spec
minkowskiTests' s (map toCCW -> pgs) = describe ("Minkowskisums on " ++ s) $
    mapM_ (uncurry minkowskiTest) [ (p,q) | p <- pgs, q <- pgs ]


minkowskiTest     ::  (Fractional r, Ord r, Eq p, Show r, Show p)
                  => ConvexPolygon p r -> ConvexPolygon p r -> Spec
minkowskiTest p q = it "minkowskisum" $
  F (p,q) (minkowskiSum p q) `shouldBe` F (p,q) (naiveMinkowski p q)

naiveMinkowski     :: (Fractional r, Ord r)
                   => ConvexPolygon p r -> ConvexPolygon q r -> ConvexPolygon (p, q) r
naiveMinkowski p q = over (simplePolygon.unsafeOuterBoundaryVector) bottomMost
                   . toCCW . convexHull . NonEmpty.fromList
                   $ [ v .+. w | v <- p^..simplePolygon.outerBoundaryVector.traverse
                               , w <- q^..simplePolygon.outerBoundaryVector.traverse
                     ]
  where
    (v :+ ve) .+. (w :+ we) = v .+^ (toVec w) :+ (ve,we)


toCCW :: (Fractional r, Eq r) => ConvexPolygon p r -> ConvexPolygon p r
toCCW = over simplePolygon toCounterClockWiseOrder

data F a b = F a b deriving (Show)

instance Eq b => Eq (F a b) where
  (F _ b1) == (F _ b2) = b1 == b2


newtype CP r = CP (ConvexPolygon () r) deriving (Eq,Show)

instance (Arbitrary r, Fractional r, Ord r) => Arbitrary (CP r) where
  arbitrary =  CP . toCCW <$> suchThat (convexHull <$> arbitrary)
                              (\p -> p^.simplePolygon.outerBoundaryVector.to length > 2)
