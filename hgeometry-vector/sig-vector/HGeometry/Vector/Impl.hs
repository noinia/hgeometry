{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Impl
  ( sameDirection
  , scalarMultiple
  , isScalarMultipleOf
  ) where

import           Control.Lens
import           D
import           Data.Semigroup
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.TypeLits (natVal)
import           Data.Proxy
import           HGeometry.Vector.Class
import           R
import           Vector

--------------------------------------------------------------------------------


-- | 'isScalarmultipleof u v' test if v is a scalar multiple of u.
--
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 10
-- True
-- >>> Vector3 1 1 2 `isScalarMultipleOf` Vector3 10 10 20
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 10 1
-- False
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 (-1) (-1)
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.1
-- True
-- >>> Vector2 1 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 11.1 11.2
-- False
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 2
-- True
-- >>> Vector2 2 1 `isScalarMultipleOf` Vector2 4 0
-- False
-- >>> Vector3 2 1 0 `isScalarMultipleOf` Vector3 4 0 5
-- False
-- >>> Vector3 0 0 0 `isScalarMultipleOf` Vector3 4 0 5
-- True
isScalarMultipleOf       :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                         => Vector D R -> Vector D R  -> Bool
u `isScalarMultipleOf` v = let d' = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d'*d'
{-# INLINE isScalarMultipleOf #-}

-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: ( Num R, Eq R,  R ~ IxValue (Vector D R)
                     ) => Vector D R -> Vector D R -> Bool
sameDirection u v = getAll $ foldMapZip (\ux vx -> All $ signum ux == signum vx) u v
{-# INLINE sameDirection #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                   => Vector D R -> Vector D R -> Maybe R
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v
  where
    allZero = allOf components (== 0)
{-# INLINABLE scalarMultiple #-}

data ScalarMultiple r = No | Maybe | Yes r deriving (Eq,Show)

instance Eq r => Semigroup (ScalarMultiple r) where
  No      <> _       = No
  _       <> No      = No
  Maybe   <> x       = x
  x       <> Maybe   = x
  (Yes x) <> (Yes y)
     | x == y               = Yes x
     | otherwise            = No

instance Eq r => Monoid (ScalarMultiple r) where
  mempty = Maybe
  mappend = (<>)

-- | Actual implementation of scalarMultiple
scalarMultiple'      :: (Eq R, Fractional R, R ~ IxValue (Vector D R))
                     => Vector D R -> Vector D R -> Maybe R
scalarMultiple' u v = g $ foldMapZip f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * unboxed vectors

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (Vector D R) = MV_VectorD (UMV.MVector s R)
newtype instance UV.Vector     (Vector D R) = V_VectorD  (UV.Vector     R)

d :: Int
d = fromInteger $ natVal (Proxy @D)

instance ( GMV.MVector UMV.MVector R
         , VectorLike_ (Vector D R)
         , R ~ IxValue (Vector D R)
         ) => GMV.MVector UMV.MVector (Vector D R) where

  basicLength (MV_VectorD v) = GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VectorD v) = MV_VectorD $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VectorD v) (MV_VectorD v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_VectorD <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VectorD v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VectorD v) i = generateA $ \j -> GMV.basicUnsafeRead v (d*i+j)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VectorD v) i w = imapMOf_ components f w
    where
      f j x = GMV.basicUnsafeWrite v (d*i+j) x
  {-# INLINE basicUnsafeWrite #-}


instance ( GV.Vector UV.Vector R
         , VectorLike_ (Vector D R)
         , R ~ IxValue (Vector D R)
         ) => GV.Vector UV.Vector (Vector D R) where

  basicUnsafeFreeze (MV_VectorD mv) = V_VectorD <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VectorD v) = MV_VectorD <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VectorD v) = GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VectorD v) = V_VectorD $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VectorD v) i = generateA $ \j -> GV.basicUnsafeIndexM v (d*i+j)
  {-# INLINE basicUnsafeIndexM #-}

instance ( UV.Unbox R, VectorLike_ (Vector D R), R ~ IxValue (Vector D R)
         ) => UV.Unbox (Vector D R) where
