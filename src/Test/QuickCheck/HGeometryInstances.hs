{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.HGeometryInstances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for most geometry types in HGeometry
--
--------------------------------------------------------------------------------
module Test.QuickCheck.HGeometryInstances where

import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry hiding (vector)
import           Data.Geometry.Box
import           Data.PlanarGraph
import qualified Data.PlanarGraph as PlanarGraph
import           Data.Geometry.SubLine
import           Data.OrdSeq (OrdSeq, fromListByOrd)
import           Data.Proxy
import qualified Data.LSeq as LSeq
import           GHC.TypeLits
import           Test.QuickCheck

--------------------------------------------------------------------------------

-- instance Arbitrary a => Arbitrary (NonEmpty.NonEmpty a) where
--   arbitrary = NonEmpty.fromList <$> listOf1 arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (OrdSeq a) where
  arbitrary = fromListByOrd <$> arbitrary

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


instance (KnownNat n, Arbitrary a) => Arbitrary (LSeq.LSeq n a) where
  arbitrary = (\s s' -> LSeq.promise . LSeq.fromList $ s <> s')
            <$> vector (fromInteger . natVal $ (Proxy :: Proxy n))
            <*> arbitrary

instance (Arbitrary r, Arity d) => Arbitrary (Vector d r) where
  arbitrary = vectorFromListUnsafe <$> infiniteList

instance (Arbitrary r, Arity d) => Arbitrary (Point d r) where
  arbitrary = Point <$> arbitrary

instance (Arbitrary r, Arity d, Num r, Eq r) => Arbitrary (Line d r) where
  arbitrary = do p <- arbitrary
                 q <- suchThat arbitrary (/= p)
                 return $ lineThrough p q

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


instance (Arbitrary r, Arbitrary p, Arbitrary s, Arity d, Ord r, Ord s, Ord p, Num r)
         => Arbitrary (SubLine d p s r) where
  arbitrary = SubLine <$> arbitrary <*> arbitrary


instance (Arbitrary r, Arbitrary p, Arity d) => Arbitrary (LineSegment d p r) where
  arbitrary = LineSegment <$> arbitrary <*> arbitrary



instance Arbitrary (Arc s) where
  arbitrary = Arc <$> (arbitrary `suchThat` (>= 0))

instance Arbitrary Direction where
  arbitrary = (\b -> if b then PlanarGraph.Positive else Negative) <$> arbitrary

instance Arbitrary (Dart s) where
  arbitrary = Dart <$> arbitrary <*> arbitrary
