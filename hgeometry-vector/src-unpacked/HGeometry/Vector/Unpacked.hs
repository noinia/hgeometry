--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Unpacked
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Unpacked low dimensional vectors
--
--------------------------------------------------------------------------------
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
-- * 1 Dimensional Vectors

-- | 1D vectors
newtype instance Vector 1 R = V_1 V1.Vec
  deriving newtype (Eq,Ord,Generic,NFData)

-- | Construct a 1 dimensional vector
pattern Vector1   :: R -> Vector 1 R
pattern Vector1 x = V_1 (V1.Single x)
{-# COMPLETE Vector1 #-}

_V1 :: Iso' (Vector 1 R) V1.Vec
_V1 = iso (\(V_1 v) -> v) V_1

instance VectorLike_ (Vector 1 R) where
  generateA f = V_1 <$> generateA f
  {-# INLINE generateA #-}
  components = components' _V1
  {-# INLINE components #-}
  unsafeComponent i = unsafeComponent' _V1 i
  {-# INLINE unsafeComponent #-}

instance Additive_ (Vector 1 R) where
  zero = V_1 zero
  {-# INLINE zero #-}
  liftU2 f (V_1 v) (V_1 v')  = V_1 $ liftU2 f v v'
  {-# INLINE liftU2 #-}
  liftI2 f (V_1 v) (V_1 v')  = V_1 $ liftI2 f v v'
  {-# INLINE liftI2 #-}
  liftI2A f (V_1 v) (V_1 v') = V_1 <$> liftI2A f v v'
  {-# INLINE liftI2A #-}

-- we implement the V1 stuff manually; since the VD setup requires the cons library

--------------------------------------------------------------------------------
-- | Convenience constructors

-- | Construct a 2 dimensional vector
pattern Vector2     :: R -> R -> Vector 2 R
pattern Vector2 x y = V2.V_D (V2.Cons x
                                      (V1.Single y)
                             )
{-# COMPLETE Vector2 #-}

-- | Construct a 3 dimensional vector
pattern Vector3       :: R -> R -> R -> Vector 3 R
pattern Vector3 x y z = V3.V_D (V3.Cons x
                                        (V2.Cons y (V1.Single z))
                               )
{-# COMPLETE Vector3 #-}

-- | Construct a 4 dimensional vector
pattern Vector4         :: R -> R -> R -> R -> Vector 4 R
pattern Vector4 x y z w = V4.V_D (V4.Cons x
                                   (V3.Cons y
                                     (V2.Cons z (V1.Single w)))
                                 )
{-# COMPLETE Vector4 #-}

--------------------------------------------------------------------------------
-- * Additional Functionality


-- -- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- -- iff scalarMultiple' u v == Just lambda, with lambda > 0
-- --
-- -- pre: u and v are colinear, u and v are non-zero
-- sameDirection     :: forall vector vector' d r. ( Vector_ vector  d r, Vector_ vector' d r
--                      , Num r, Eq r
--                      ) => vector -> vector' -> Bool
-- sameDirection u v = andOf components $
--     vZipWith @_ @_ @(ListVector.ListVector d Bool) (\ux vx -> signum ux == signum vx) u v

-- -- | 'isScalarmultipleof u v' test if v is a scalar multiple of u.
-- --
-- -- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 10
-- -- True
-- -- >>> Vector3 1 1 2 `isScalarMultipleOf` Vector3 10 10 20
-- -- True
-- -- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 1
-- -- False
-- -- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 (-1) (-1)
-- -- True
-- -- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.1
-- -- True
-- -- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- -- False
-- -- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- -- False
-- -- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 2
-- -- True
-- -- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 0
-- -- False
-- -- >>> Vector3 2 1 0 `isScalarMultipleOf` Vector3 4 0 5
-- -- False
-- -- >>> Vector3 0 0 0 `isScalarMultipleOf` Vector3 4 0 5
-- -- True
-- isScalarMultipleOf       :: (Eq r, Fractional r
--                             , Vector_ vector d r, Metric_ vector
--                             )
--                          => vector -> vector  -> Bool
-- u `isScalarMultipleOf` v = let d = u `dot` v
--                                num = quadrance u * quadrance v
--                            in num == 0 || num == d*d
-- -- u `isScalarMultipleOf` v = isJust $ scalarMultiple u v


-- -- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
-- scalarMultiple     :: (Eq r, Fractional r, Vector_ vector d r)
--                    => vector -> vector -> Maybe r
-- scalarMultiple u v
--       | allZero u || allZero v = Just 0
--       | otherwise              = scalarMultiple' u v

-- allZero :: (Eq r, Num r, Vector_ vector d r) => vector -> Bool
-- allZero = allOf components (== 0)

-- data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

-- instance Eq r => Semigroup (ScalarMultiple r) where
--   No      <> _       = No
--   _       <> No      = No
--   Maybe   <> x       = x
--   x       <> Maybe   = x
--   (Yes x) <> (Yes y)
--      | x == y               = Yes x
--      | otherwise            = No


-- instance Eq r => Monoid (ScalarMultiple r) where
--   mempty = Maybe
--   mappend = (<>)

-- -- | Actual implementation of scalarMultiple
-- scalarMultiple'      :: forall vector d r. (Eq r, Fractional r, Vector_ vector d r)
--                      => vector -> vector -> Maybe r
-- scalarMultiple' u v = g . foldOf components
--                     $ vZipWith @_ @_ @(ListVector.ListVector d (ScalarMultiple r)) f u v
--   where
--     f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
--     f _  0  = No      -- Not a scalar multiple
--     f ui vi = Yes $ ui / vi -- can still be a scalar multiple

--     g No      = Nothing
--     g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
--     g (Yes x) = Just x

-- -- {-# SPECIALIZE
-- --     scalarMultiple' :: (Eq r, Fractional r) => Vector 2 r -> Vector 2 r -> Maybe r #-}


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
