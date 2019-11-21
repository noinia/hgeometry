{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Polygon.Convex.ConvexSpec where

import           Algorithms.Geometry.ConvexHull.GrahamScan (convexHull, lowerHull)
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon (extremesLinear, fromPoints)
import           Data.Geometry.Polygon.Convex
import qualified Data.Geometry.Polygon.Convex.LowerTangent as LowerT
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Traversable (traverse)
import           Data.Util
import           Test.Hspec
import           Test.QuickCheck (Arbitrary(..), property, suchThat)
import           Test.QuickCheck.Instances ()

-- getVertices :: ConvexPolygon p r -> C.CSeq (Point 2 r :+ p)
-- getVertices = view (simplePolygon.outerBoundary)

-- -- | Rotate to the rightmost point (rightmost and topmost in case of ties)
-- rightMost    :: Ord r => C.CSeq (Point 2 r :+ p) -> C.CSeq (Point 2 r :+ p)
-- rightMost xs = let m = F.maximumBy (comparing (^.core)) xs in rotateTo' m xs

-- -- | Rotate to the leftmost point (and bottommost in case of ties)
-- leftMost    :: Ord r => C.CSeq (Point 2 r :+ p) -> C.CSeq (Point 2 r :+ p)
-- leftMost xs = let m = F.minimumBy (comparing (^.core)) xs in rotateTo' m xs

-- rotateTo'   :: Eq a => (a :+ b) -> C.CSeq (a :+ b) -> C.CSeq (a :+ b)
-- rotateTo' x = fromJust . C.findRotateTo (coreEq x)
--   where
--     coreEq = (==) `on` (^.core)

mkLowerTangentSets       :: (r ~ Int)
                         => NonEmpty (Point 2 r :+ ()) -> NonEmpty (Point 2 r :+ ())
                         -> (ConvexPolygon () r, ConvexPolygon () r)
mkLowerTangentSets lp rp = (lowerHull' lp, lowerHull' rp')
  where
    lowerHull' = ConvexPolygon . fromPoints . F.toList . lowerHull
    rp' = (\p -> p&core.xCoord %~ (+ w)) <$> rp
    w = maximum . fmap (^.core.xCoord) $ lp

spec :: Spec
spec = do testCases "test/Data/Geometry/Polygon/Convex/convexTests.ipe"
          it "LowerTangents the same" $
            property $ \lp rp -> let (lh,rh) = mkLowerTangentSets lp rp
                                 in lowerTangent lh rh == LowerT.lowerTangent lh rh


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading ConvexTests file" $
                   expectationFailure . unwords $
                     [ "Failed to read ipe file", show fp, ":", show e]
    Right tcs -> do mapM_ toSpec tcs
                    minkowskiTests $ map _polygon tcs

data TestCase r = TestCase { _polygon    :: ConvexPolygon () r
                           }
                  deriving (Show)

toSingleSpec        :: (Num r, Ord r, Show r)
                    => ConvexPolygon q r -> Vector 2 r -> SpecWith ()
toSingleSpec poly u = it msg $
  -- test that the reported extremes are equally far in direction u
    F.all allEq (unzip [extremes u poly, extremesLinear u (poly^.simplePolygon)])
    `shouldBe` True
  where
    allEq (p:ps) = all (\q -> cmpExtreme u p q == EQ) ps
    msg = "Extremes test with direction " ++ show u

-- | generates 360 vectors "equally" spaced/angled
directions :: Num r => [Vector 2 r]
directions = map (fmap toRat . uncurry Vector2 . (cos &&& sin) . toRad) ([0..359] :: [Double])
  where
    toRad i = i * (pi / 180)
    toRat x = fromIntegral . round $ 100000 * x

toSpec                 :: (Num r, Ord r, Show r) => TestCase r -> SpecWith ()
toSpec (TestCase poly) = do
                           describe "Extreme points; binsearch same as linear" $
                             mapM_ (toSingleSpec poly) directions




readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
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
naiveMinkowski p q = over (simplePolygon.outerBoundary) bottomMost
                   . toCCW . convexHull . NonEmpty.fromList
                   $ [ v .+. w | v <- p^..simplePolygon.outerBoundary.traverse
                               , w <- q^..simplePolygon.outerBoundary.traverse
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
                              (\p -> p^.simplePolygon.outerBoundary.to length > 2)
