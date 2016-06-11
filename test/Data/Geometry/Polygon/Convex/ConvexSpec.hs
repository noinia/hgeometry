module Data.Geometry.Polygon.ConvexSpec where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon (extremesLinear)
import           Data.Geometry.Polygon.Convex
import           Data.Traversable (traverse)
import           Test.Hspec



spec :: Spec
spec = testCases "test/Data/Geometry/Polygon/Convex/convexTests.ipe"

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading ConvexTests file" $
                   expectationFailure . unwords $
                     [ "Failed to read ipe file", show fp, ":", show e]
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _polygon    :: ConvexPolygon () r
                           }
                  deriving (Show)

toSingleSpec        :: (Num r, Ord r, Show r)
                    => ConvexPolygon q r -> Vector 2 r -> SpecWith ()
toSingleSpec poly u = it msg $
  -- test that the reported extremes are equally far in direction u
    F.all allEq (unzip [extremes u poly, extremesLinear u poly]) `shouldBe` True
  where
    allEq (p:ps) = all (\q -> cmpExtreme u p q == EQ) ps
    msg = "Extremes test with direction " ++ show u

-- | generates 360 vectors "equally" spaced/angled
directions :: Num r => [Vector 2 r]
directions = map (fmap toRat . uncurry v2 . (cos &&& sin) . toRad) ([0..359] :: [Double])
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
    f page = [ TestCase poly | (poly :+ _) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
