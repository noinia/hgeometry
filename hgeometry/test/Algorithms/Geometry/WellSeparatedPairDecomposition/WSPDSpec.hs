module Algorithms.Geometry.WellSeparatedPairDecomposition.WSPDSpec where

import           Algorithms.Geometry.Diameter.Naive
import           Algorithms.Geometry.WellSeparatedPairDecomposition.Types
import           Algorithms.Geometry.WellSeparatedPairDecomposition.WSPD
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.LSeq as LSeq
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Test.Hspec
import           Test.Util
import           GHC.TypeLits

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  reIndexTest
  distributePointsTest

reIndexTest :: Spec
reIndexTest = describe "ReIndex tests" $ do
    it "simple input reordering " $ do
      reIndexPoints input `shouldBe` output
  where
    input = Vector2 (ptSeq [ origin :+ 1, Point2 1 1 :+ 100, Point2 5 5 :+ 101 ])
                    (ptSeq [ Point2 1 1 :+ 100, Point2 5 5 :+ 101, origin :+ 1 ])
    output = Vector2 (ptSeq [ origin :+ 0, Point2 1 1 :+ 1, Point2 5 5 :+ 2 ])
                     (ptSeq [ Point2 1 1 :+ 1, Point2 5 5 :+ 2, origin :+ 0 ])






distributePointsTest :: Spec
distributePointsTest = describe "DistributePoints tests" $ do
    it "distributePoints' on a single list " $ do
      distributePoints' 3 levels input `shouldBe` output
    it "distributePoints on multiple lists" $ do
      distributePoints 3 levels (Vector2 input input) `shouldBe` output'

  where
    levels = V.fromList [Just $ Level 0 (Just 2),Just $ Level 1 (Just 1), Nothing]
    input  = ptSeq [ origin :+ 0, Point2 1 1 :+ 1, Point2 2 2 :+ 2]
    output = V.fromList [ ptSeq [origin :+ 0]
                        , ptSeq [Point2 1 1 :+ 1]
                        , ptSeq [Point2 2 2 :+ 2]
                        ]
    output' = fmap (\pts -> Vector2 pts pts) output

    --     input = v2 (f [ origin :+ 1, Point2 1 1 :+ 100, Point2 5 5 :+ 101 ])
--                (f [ Point2 1 1 :+ 100, Point2 5 5 :+ 101, origin :+ 1 ])
--     output = v2 (f [ origin :+ 0, Point2 1 1 :+ 1, Point2 5 5 :+ 2 ])
--                 (f [ Point2 1 1 :+ 1, Point2 5 5 :+ 2, origin :+ 0 ])

--     f =  LSeq.fromNonEmpty . NonEmpty.fromList . map (&extra %~ ext)

ptSeq = LSeq.fromNonEmpty . NonEmpty.fromList . map (&extra %~ ext)

-- coversAll

points1 :: NonEmpty.NonEmpty (Point 2 Double :+ ())
points1 = ext <$> NonEmpty.fromList [Point2 0 0, Point2 1 1, Point2 2 100, Point2 3 101]


-- | Computes all pairs of points that are uncovered by the WSPD with separation s
uncovered         :: (Floating r, Ord r, Arity d, Arity (d+1), Ord p)
                  => [Point d r :+ p] -> r -> SplitTree d p r a -> [(Point d r :+ p, Point d r :+ p)]
uncovered pts s t = Set.toList $ allPairs `Set.difference` covered
  where
    allPairs = Set.fromList [ (p,q) | p <- pts, q <- pts, p < q ]
    covered  = Set.unions [ mkSet as bs | (as,bs) <- wellSeparatedPairs s t]

    mkSet as bs = Set.fromList [ (min a b,max a b) | a <- F.toList as, b <- F.toList bs]

-- | Naively check if a WSP pair is actually well separated with respect to
-- separation s. i.e. computes the maximum diameter of as and bs, and then
-- tests by brute force if all pairs (a,b) from different sets are at distance
-- at least s times the maximum diameter.
isWellSeparated           :: (Floating r, Ord r, Arity d) => r -> WSP d p r a -> Bool
isWellSeparated s (as,bs) =
    and [ euclideanDist (a^.core) (b^.core) >= s*r | a <- F.toList as, b <- F.toList bs ]
  where
    r = (/2) . maximum . map (diameter . F.toList) $ [as,bs]



allCoveredTest = describe
