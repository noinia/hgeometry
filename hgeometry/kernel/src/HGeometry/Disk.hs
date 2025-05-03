{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ball.Diametral
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A ball represented by its two diametral points
--
--------------------------------------------------------------------------------
module HGeometry.Disk
  ( DiskByPoints(..)
  ) where

import Control.Lens
import Data.Foldable1
import HGeometry.Ball.BoundaryPoints
import HGeometry.Ball.Class
import HGeometry.Ball.Diametral
import HGeometry.Boundary
import HGeometry.Intersection
import HGeometry.Point
import HGeometry.Properties

--------------------------------------------------------------------------------
-- | Disks are either diametral, or specified by three points
data DiskByPoints point = DiametralDisk !(DiametralBall point)
                        | DiskByPoints  !(BallByPoints' 3 point)
                        deriving stock (Show,Eq,Foldable,Functor)

type instance Dimension (DiskByPoints point) = Dimension point -- this better be 2
type instance NumType   (DiskByPoints point) = NumType point

makePrisms ''DiskByPoints


instance Foldable1 DiskByPoints where
  foldMap1 f = \case
    DiametralDisk d -> foldMap1 f d
    DiskByPoints d  -> foldMap1 f d

instance Traversable DiskByPoints where
  traverse f = \case
    DiametralDisk d -> DiametralDisk <$> traverse f d
    DiskByPoints d  -> DiskByPoints  <$> traverse f d

instance Traversable1 DiskByPoints where
  traverse1 f = \case
    DiametralDisk d -> DiametralDisk <$> traverse1 f d
    DiskByPoints d  -> DiskByPoints  <$> traverse1 f d


instance (Point_ point 2 r, Fractional r) => Ball_ (DiskByPoints point) (Point 2 r) where
  squaredRadius = to $ \case
    DiametralDisk d -> d^.squaredRadius
    DiskByPoints d  -> d^.squaredRadius

--------------------------------------------------------------------------------

instance Point_ point 2 r => HasInBall (DiskByPoints point) where
  inBall q = \case
    DiametralDisk d -> inBall q d
    DiskByPoints d  -> inBall q d

type instance Intersection (Point 2 r) (DiskByPoints point) = Maybe (Point 2 r)

instance ( Point_ point 2 r, Ord r, Num r
         ) => (Point 2 r) `HasIntersectionWith` (DiskByPoints  point) where
  intersects q b = q `inBall` b /= Outside

instance ( Point_ point 2 r, Ord r, Num r
         ) => (Point 2 r) `IsIntersectableWith` (DiskByPoints point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing

--------------------------------------------------------------------------------


instance (Point_ point 2 r, Fractional r) => HasCenter  (DiskByPoints point) (Point 2 r) where
  center = lens (\case
                    DiametralDisk d -> d^.center
                    DiskByPoints d  -> d^.center
                )
                (\case
                    DiametralDisk d -> DiametralDisk . flip (set center) d
                    DiskByPoints d  -> DiskByPoints  . flip (set center) d
                )
