{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.Unpacked
  ( Vector(Vector1, Vector2, Vector3, Vector4)
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R
import qualified V1
import qualified V2
import qualified V3
import qualified V4

--------------------------------------------------------------------------------

-- | 1D vectors
newtype instance Vector 1 R = V_1 V1.Vec
  deriving newtype (Eq,Ord,Generic,Additive_,NFData)

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
-- | Convenience constructors

-- | Construct a 2 dimensional vector
pattern Vector2     :: R -> R -> Vector 2 R
pattern Vector2 x y = V2.V_D (V2.Cons x
                                      (V1.Single y)
                             )

-- | Construct a 3 dimensional vector
pattern Vector3       :: R -> R -> R -> Vector 3 R
pattern Vector3 x y z = V3.V_D (V3.Cons x
                                        (V2.Cons y (V1.Single z))
                               )

-- | Construct a 4 dimensional vector
pattern Vector4         :: R -> R -> R -> R -> Vector 4 R
pattern Vector4 x y z w = V4.V_D (V4.Cons x
                                   (V3.Cons y
                                     (V2.Cons z (V1.Single w)))
                                 )



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
