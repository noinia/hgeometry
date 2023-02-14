--------------------------------------------------------------------------------
-- |
-- Module      :  VectorD
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- D-dimensional vector instance for numtype R
--
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-orphans #-}
module VectorD
  ( Vector(..)
  , Out.Vec(..)
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R
import           D
import qualified Out
import qualified In


-- import qualified Data.Vector.Generic as GV
-- import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
-- import qualified Data.Vector.Generic.Mutable as GVM
-- import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UVM


--------------------------------------------------------------------------------

-- | vectors
newtype instance Vector D R = V_D Out.Vec
  deriving newtype (Eq,Ord,Generic,NFData)

_VD :: Iso' (Vector D R) Out.Vec
_VD = iso (\(V_D v) -> v) V_D

instance (IxValue In.Vec ~ R) => VectorLike_ (Vector D R) where
  generateA f = V_D <$> generateA f
  {-# INLINE generateA #-}
  components = components' _VD
  {-# INLINE components #-}
  component' i = component'' _VD i
  {-# INLINE component' #-}

instance (IxValue In.Vec ~ R) => Additive_ (Vector D R) where
  zero = V_D zero
  {-# INLINE zero #-}
  liftU2 f (V_D v) (V_D v')  = V_D $ liftU2 f v v'
  {-# INLINE liftU2 #-}
  liftI2 f (V_D v) (V_D v')  = V_D $ liftI2 f v v'
  {-# INLINE liftI2 #-}
  liftI2A f (V_D v) (V_D v') = V_D <$> liftI2A f v v'
  {-# INLINE liftI2A #-}

--------------------------------------------------------------------------------
-- * Helpers

-- | implementation of component
components'    :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
               => Iso' vector vecImpl -> IndexedTraversal1' Int vector R
components' is = is.components
{-# INLINE components' #-}

-- | implementation of component'
component''      :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
                      => Iso' vector vecImpl -> Int -> IndexedTraversal' Int vector R
component'' is i = is.component' i
{-# INLINE component'' #-}
