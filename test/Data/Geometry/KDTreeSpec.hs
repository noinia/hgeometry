{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.KDTreeSpec where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Box
import           Data.Geometry.KDTree
import qualified Data.Seq as Seq
import qualified Data.Set as Set
import           GHC.TypeLits
import           Test.QuickCheck.HGeometryInstances()
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

naive    :: (Arity d, Ord r) => Box d q r -> [Point d r :+ p] -> [Point d r :+ p]
naive qr = filter (\(p :+ _) ->  p `intersects` qr)

sameAsNaive         :: (Ord r, Ord p, Arity d)
                    => [Point d r :+ p] -> KDTree d p r -> Box d q r -> Bool
sameAsNaive pts t q = Set.fromList (searchKDTree q t) == Set.fromList (naive q pts)

allSameAsNaive     :: (Ord r, Ord p, Arity d, KnownNat d, Index' 0 d, Foldable f)
                   => f (Point d r :+ p) -> [Box d () r] -> Bool
allSameAsNaive pts = let pts' = F.toList pts
                     in  all (sameAsNaive pts' $ buildKDTree pts')

allSame :: (Arity d, Eq a) => Vector d a -> Bool
allSame v = case F.toList v of
              []     -> True
              (x:xs) -> all (== x) xs

-- newtype Pts n d r = Pts (PointSet (Seq.LSeq n) d () r)
-- deriving instance (Arity d, Show r) => Show (Pts n d r)

-- instance (KnownNat n, Arity d, KnownNat d, Arbitrary r, Ord r) => Arbitrary (Pts n d r) where
--   arbitrary = Pts . toPointSet . Seq.toNonEmpty <$> arbitrary


spec :: Spec
spec = do
  describe "splitOn" $ do
    it "quickheck: left set same points" $
      property $ \c (pts :: Seq.LSeq 2 (Point 2 Int :+ ())) ->
                   let (l,_,_) = splitOn (toEnum c) (toPointSet pts)
                   in allSame . fmap (Set.fromList . F.toList) $ l
    it "quickheck: right set same points" $
      property $ \c (pts :: Seq.LSeq 2 (Point 2 Int :+ ())) ->
                   let (_,_,r) = splitOn (toEnum c) (toPointSet pts)
                   in allSame . fmap (Set.fromList . F.toList) $ r
  describe "Same as Naive" $ do
    it "quickcheck 1d" $
      property $ \(pts :: Set.Set (Point 1 Int :+ ())) -> allSameAsNaive pts
    it "quickcheck 2d" $
      property $ \(pts :: Set.Set (Point 2 Int :+ ())) -> allSameAsNaive pts
    it "quickcheck 3d" $
      property $ \(pts :: Set.Set (Point 3 Int :+ ())) -> allSameAsNaive pts
    it "quickcheck 8d" $
      property $ \(pts :: Set.Set (Point 8 Int :+ ())) -> allSameAsNaive pts


-- pts = map ext [point2 (-2) 2, point2 5 (-1)]
-- boxx = box (ext $ point2 3 (-4)) (ext $ point2 5 4)
