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
import HGeometry.Ball.BoundaryPoints
import HGeometry.Ball.Class
import HGeometry.Ball.Diametral
import HGeometry.Point
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | Disks are either diametral, or specified by three points
data DiskByPoints point = DiametralDisk !(DiametralBall point)
                        | DiskByPoints  !(BallByPoints point)
                        deriving stock (Show,Eq,Foldable,Functor)

instance Foldable1 DiskByPoints

type instance Dimension (DiskByPoints point) = Dimension point -- this better be 2
type instance NumType   (DiskByPoints point) = NumType point



instance HasInBall (DiskByPoints point) 2 r where
  inBall q = \case
    DiametralDisk d -> inBall q d
    DiskByPoints d  -> inBall q d
