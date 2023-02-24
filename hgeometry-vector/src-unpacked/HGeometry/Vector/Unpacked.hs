{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Unpacked
  (

  ) where

import Control.DeepSeq
import HGeometry.Vector.Unpacked.Def
import R

--------------------------------------------------------------------------------


newtype instance UnpackedVector 1 = V1 R

deriving stock instance Show R => Show (UnpackedVector 1)
deriving stock instance Read R => Read (UnpackedVector 1)

deriving newtype instance Eq R => Eq (UnpackedVector 1)
deriving newtype instance Ord R => Ord (UnpackedVector 1)
deriving newtype instance NFData R => NFData (UnpackedVector 1)


-- generateA :: forall f d. ( Applicative f
--                          , KnownNat d
--              ) => (f -> f R) -> f (UnpackedVector d)
-- generateA = let d = natVal (Proxy @d)
--             in
