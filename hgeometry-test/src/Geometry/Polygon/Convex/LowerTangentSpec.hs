module Geometry.Polygon.Convex.LowerTangentSpec (spec) where


import           Algorithms.Geometry.ConvexHull.GrahamScan (lowerHull)
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable                             as F
import           Geometry                             hiding (vector)
import           Geometry.Polygon
import           Geometry.Polygon.Convex
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Test.Hspec
import           Test.QuickCheck                           (Arbitrary (..), listOf, suchThat)
import           Test.QuickCheck.Instances                 ()

spec :: Spec
spec = pure ()
  -- I've disabled these tests for now since the old implementation was not very
  -- robust to colinear points; having degenerate polygons etc. Hence,
  -- on the testcases generated that implementation loops.
  -- do it "LowerTangents the same" $ do
  --           property $ \(Hulls lh rh) -> traceShow (lh,rh) $
  --               lowerTangent lh rh == LowerT.lowerTangent lh rh
  --         it "UpperTangents the same" $
  --           property $ \(Hulls lh rh) ->
  --               upperTangent lh rh == LowerT.upperTangent lh rh

data Hulls = Hulls (ConvexPolygon () Word) (ConvexPolygon () Word) deriving (Show,Eq)

instance Arbitrary Hulls where
  arbitrary = do lh <- left
                 rh <- right (xMax lh)
                 pure $ Hulls lh rh
    where
      xMax = maximum . fmap (^.core.xCoord) . (^.simplePolygon.outerBoundaryVector)
      lowerHull' = ConvexPolygon . fromPoints . reverse . F.toList . lowerHull
      left = do p    <- arbitrary
                q    <- suchThat arbitrary (\a -> a^.core.xCoord > p^.core.xCoord)
                rest <- arbitrary
                pure . lowerHull' $ p :| (q : rest)
      right x = do p    <- suchThat arbitrary  (\a -> a^.core.xCoord > x)
                   q    <- suchThat arbitrary  (\a -> a^.core.xCoord > p^.core.xCoord)
                   rest <- listOf $ suchThat arbitrary  (\a -> a^.core.xCoord > x)
                   pure . lowerHull' $ p :| (q : rest)

-- newtype Hull = Hull (NonEmpty (Point 2 Rational :+ ())) deriving (Show,Eq)

-- -- make sure we generate at least two points
-- instance Arbitrary Hull where
--   arbitrary = (\a (Point2 x y) rs -> Hull . NonEmpty.fromList $ a : (ext $ Point2 (x+1) y) : rs)
--               <$> arbitrary <*> arbitrary <*> arbitrary

-- mkLowerTangentSets       :: (Ord r, Fractional r, Show r)
--                          => NonEmpty (Point 2 r :+ ()) -> NonEmpty (Point 2 r :+ ())
--                          -> (ConvexPolygon () r, ConvexPolygon () r)
-- mkLowerTangentSets lp rp = traceShowId $ (lowerHull' lp, lowerHull' rp')
--   where
--     lowerHull' = ConvexPolygon . fromPoints . F.toList . lowerHull
--     rp' = (\p -> p&core.xCoord %~ (+ w)) <$> rp
--     w = (1 + ) . maximum . fmap (^.core.xCoord) $ lp

-- mkUpperTangentSets       :: (Ord r, Fractional r)
--                          => NonEmpty (Point 2 r :+ ()) -> NonEmpty (Point 2 r :+ ())
--                          -> (ConvexPolygon () r, ConvexPolygon () r)
-- mkUpperTangentSets lp rp = (upperHull' lp, upperHull' rp')
--   where
--     upperHull' = ConvexPolygon . fromPoints . F.toList . upperHull
--     rp' = (\p -> p&core.xCoord %~ (+ w)) <$> rp
--     w = (1 + ) . maximum . fmap (^.core.xCoord) $ lp
