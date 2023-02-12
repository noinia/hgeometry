{-# OPTIONS_GHC -Wno-orphans #-}
module Vector
  ( Vector(Vector1, Vector2)
  ) where

import           Control.Lens
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R
import qualified V1
import qualified V2
import qualified V3

--------------------------------------------------------------------------------

-- | 1D vectors
newtype instance Vector 1 R = V_1 V1.Vec
  deriving newtype (Eq,Ord,Generic)

-- | Construct a 1 dimensional vector
pattern Vector1   :: R -> Vector 1 R
pattern Vector1 x = V_1 (V1.Single x)

_V1 :: Iso' (Vector 1 R) V1.Vec
_V1 = iso (\(V_1 v) -> v) V_1

instance VectorLike_ (Vector 1 R) where
  components = components' _V1
  {-# INLINE components #-}
  unsafeComponent i = unsafeComponent' _V1 i
  {-# INLINE unsafeComponent #-}

--------------------------------------------------------------------------------
-- | 2D vectors
newtype instance Vector 2 R = V_2 V2.Vec
  deriving newtype (Eq,Ord,Generic)

-- | Construct a 2 dimensional vector
pattern Vector2     :: R -> R -> Vector 2 R
pattern Vector2 x y = V_2 (V2.Cons x (V1.Single y))

_V2 :: Iso' (Vector 2 R) V2.Vec
_V2 = iso (\(V_2 v) -> v) V_2

instance VectorLike_ (Vector 2 R) where
  components = components' _V2
  {-# INLINE components #-}
  unsafeComponent i = unsafeComponent' _V2 i
  {-# INLINE unsafeComponent #-}

--------------------------------------------------------------------------------
-- * Helpers

-- | implementation of component
components'    :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
               => Iso' vector vecImpl -> IndexedTraversal1' Int vector R
components' is = is.components
{-# INLINE components' #-}

-- | implementation of unsafeComponent
unsafeComponent'      :: (VectorLike_ vecImpl, IxValue vecImpl ~ R)
                      => Iso' vector vecImpl -> Int -> IndexedLens' Int vector R
unsafeComponent' is i = is.unsafeComponent i
{-# INLINE unsafeComponent' #-}
