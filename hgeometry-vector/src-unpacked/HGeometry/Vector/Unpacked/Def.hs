module HGeometry.Vector.Unpacked.Def
  ( UnpackedVector
  ) where

import Data.Proxy
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Vector.Class
import R

--------------------------------------------------------------------------------

data family UnpackedVector (d :: Nat)

instance 0 < d => Vector_ (UnpackedVector d) d R
