{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QuickCheck.Instances where

import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry hiding (vector)
import           Data.Geometry.Box
import           Data.Geometry.Interval
import           Data.Geometry.SubLine
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Data.Range
import           Data.Semigroup
import qualified Data.Seq as Seq
import qualified Data.Seq2 as S2
import           GHC.TypeLits
import           Test.QuickCheck

--------------------------------------------------------------------------------

-- instance Arbitrary a => Arbitrary (NonEmpty.NonEmpty a) where
--   arbitrary = NonEmpty.fromList <$> listOf1 arbitrary

instance Arbitrary a => Arbitrary (S2.Seq2 a) where
  arbitrary = S2.Seq2 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized f
    where f n | n <= 0    = pure Nil
              | otherwise = do
                              l <- choose (0,n-1)
                              Internal <$> f l <*> arbitrary <*> f (n-l-1)

instance (Arbitrary a, Arbitrary v) => Arbitrary (BinLeafTree v a) where
  arbitrary = sized f
    where f n | n <= 0    = Leaf <$> arbitrary
              | otherwise = do
                              l <- choose (0,n-1)
                              Node <$> f l <*> arbitrary <*> f (n-l-1)


instance (KnownNat n, Arbitrary a) => Arbitrary (Seq.LSeq n a) where
  arbitrary = (\s s' -> Seq.promise . Seq.fromList $ s <> s')
            <$> vector (fromInteger . natVal $ (Proxy :: Proxy n))
            <*> arbitrary

instance (Arbitrary r, Arity d) => Arbitrary (Vector d r) where
  arbitrary = vectorFromListUnsafe <$> infiniteList

instance (Arbitrary r, Arity d) => Arbitrary (Point d r) where
  arbitrary = Point <$> arbitrary

instance (Arbitrary r, Arity d, Num r) => Arbitrary (Line d r) where
  arbitrary = lineThrough <$> arbitrary <*> arbitrary

instance (Arbitrary r, Arity d, Ord r) => Arbitrary (Box d () r) where
  arbitrary = (\p (q :: Point d r) -> boundingBoxList' [p,q]) <$> arbitrary <*> arbitrary


instance Arbitrary r => Arbitrary (EndPoint r) where
  arbitrary = frequency [ (1, Open   <$> arbitrary)
                        , (9, Closed <$> arbitrary)
                        ]

instance (Arbitrary r, Ord r) => Arbitrary (Range r) where
  arbitrary = do
                l <- arbitrary
                r <- suchThat arbitrary (p l)
                return $ Range l r
   where
     p (Open l)   r = l <  r^.unEndPoint
     p (Closed l) r = l <= r^.unEndPoint


instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary

instance (Arbitrary r, Arbitrary p, Ord r, Ord p) => Arbitrary (Interval p r) where
  arbitrary = GInterval <$> arbitrary


instance (Arbitrary r, Arbitrary p, Arity d, Ord r, Ord p, Num r)
         => Arbitrary (SubLine d p r) where
  arbitrary = SubLine <$> arbitrary <*> arbitrary


instance (Arbitrary r, Arbitrary p, Arity d) => Arbitrary (LineSegment d p r) where
  arbitrary = LineSegment <$> arbitrary <*> arbitrary
