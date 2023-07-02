module Polygon.Convex.ConvexSpec
  (spec
  ) where

import           Control.Lens
-- import           Data.Coerce
import           Data.Default.Class
-- import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
-- import           HGeometry.Boundary
-- import           HGeometry.Box (boundingBox)
import           HGeometry.ConvexHull.GrahamScan (convexHull)
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           Ipe
import           Paths_hgeometry_test
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 10

instance Default (Point 2 R) where
  def = origin

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  testCases "Polygon/Convex/convexTests.ipe"

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading ConvexTests file" $
                   expectationFailure . unwords $
                     [ "Failed to read ipe file", show fp, ":", show e]
    Right tcs -> do minkowskiTests "polygons in ipe file" $ map _polygon tcs
                    -- mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: ConvexPolygon (Point 2 r)
                           }
                  deriving (Show)

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase R])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase poly | (poly :+ _) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asConvexPolygon


-- toSingleSpec        :: (Num r, Ord r, Show r)
--                     => ConvexPolygon q r -> Vector 2 r -> Expectation
-- toSingleSpec poly u =
--   -- test that the reported extremes are equally far in direction u
--     F.all allEq (unzip [extremes u poly, extremesLinear u (poly^.simplePolygon)])
--     `shouldBe` True
--   where
--     allEq ~(p:ps) = all (\q -> cmpExtreme u p q == EQ) ps

-- -- | generates 360 vectors "equally" spaced/angled
-- directions :: Num r => [Vector 2 r]
-- directions = map (fmap toRat . uncurry Vector2 . (cos &&& sin) . toRad) ([0..359] :: [Double])
--   where
--     toRad i = i * (pi / 180)
--     toRat x = fromIntegral . round $ 100000 * x

-- toSpec                 :: (Num r, Ord r, Show r) => TestCase r -> SpecWith ()
-- toSpec (TestCase poly) = do
--                            it "Extreme points; binsearch same as linear" $
--                              mapM_ (toSingleSpec poly) directions

--------------------------------------------------------------------------------

minkowskiTests       ::  (Fractional r, Ord r, Show r, Default (Point 2 r))
                     => String -> [ConvexPolygon (Point 2 r)] -> Spec
minkowskiTests s pgs = describe ("Minkowskisums on " ++ s) $
    mapM_ (uncurry minkowskiTest) [ (p,q) | p <- pgs, q <- pgs ]

minkowskiTest     ::  (Fractional r, Ord r, Show r, Default (Point 2 r))
                  => ConvexPolygon (Point 2 r) -> ConvexPolygon (Point 2 r) -> Spec
minkowskiTest p q = it "minkowskisum" $
  F (p,q) (minkowskiSum p q) `shouldBe` F (p,q) (naiveMinkowski p q)

data F a b = F a b deriving (Show)

instance Eq b => Eq (F a b) where
  (F _ b1) == (F _ b2) = b1 == b2

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

-- newtype CP r = CP (ConvexPolygon (Point 2 r)) deriving (Eq,Show)

-- instance (Arbitrary r, Fractional r, Ord r) => Arbitrary (CP r) where
--   arbitrary =  CP <$> suchThat (convexHull <$> arbitrary)
--                                (\p -> numVertices p > 2)
