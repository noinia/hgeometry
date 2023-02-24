{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Unpacked
  (

  ) where

import           Control.DeepSeq
import           D
import qualified HGeometry.Vector.Unpacked.V1 as V1
import qualified HGeometry.Vector.Unpacked.V2 as V2
import qualified HGeometry.Vector.Unpacked.V3 as V3
import qualified HGeometry.Vector.Unpacked.V4 as V4

--------------------------------------------------------------------------------

type Vector = UnpackedVector D

type family UnpackedVector d

type instance UnpackedVector 1 = V1.Vector
type instance UnpackedVector 2 = V2.Vector
type instance UnpackedVector 3 = V3.Vector
type instance UnpackedVector 4 = V4.Vector
