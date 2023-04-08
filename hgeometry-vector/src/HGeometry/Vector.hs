{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- D-dimensional Vectors
--
--------------------------------------------------------------------------------
module HGeometry.Vector
  ( Vector(..)
  , HasComponents(..)
  , module  HGeometry.Vector.Class
  , isScalarMultipleOf
  , scalarMultiple
  , sameDirection
  ) where

import           GHC.TypeNats(KnownNat, natVal)
import           Control.Lens
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           HGeometry.Vector.Class
import           HGeometry.Vector.Type

--------------------------------------------------------------------------------

-- | Given two colinar vectors, u and v, test if they point in the same direction, i.e.
-- iff scalarMultiple' u v == Just lambda, with lambda > 0
--
-- pre: u and v are colinear, u and v are non-zero
sameDirection     :: ( Additive_ vector d r
                     , Num r, Eq r
                     ) => vector -> vector -> Bool
sameDirection u v = getAll $ foldMapZip (\ux vx -> All $ signum ux == signum vx) u v
{-# INLINE sameDirection #-}

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
isScalarMultipleOf       :: ( Eq r, Num r
                            , Metric_ vector d r
                            )
                         => vector -> vector  -> Bool
u `isScalarMultipleOf` v = let d = u `dot` v
                               num = quadrance u * quadrance v
                           in num == 0 || num == d*d
-- u `isScalarMultipleOf` v = isJust $ scalarMultiple u v
{-# INLINE isScalarMultipleOf #-}

-- | scalarMultiple u v computes the scalar labmda s.t. v = lambda * u (if it exists)
scalarMultiple     :: (Eq r, Fractional r, Additive_ vector d r)
                   => vector -> vector -> Maybe r
scalarMultiple u v
      | allZero u || allZero v = Just 0
      | otherwise              = scalarMultiple' u v
  where
    -- allZero :: (Eq r, Num r, Vector_ vector d r) => vector -> Bool
    allZero = allOf components (== 0)
{-# INLINE scalarMultiple #-}

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
scalarMultiple'      :: (Eq r, Fractional r, Additive_ vector d r)
                     => vector -> vector -> Maybe r
scalarMultiple' u v = g $ foldMapZip f u v
  where
    f 0  0  = Maybe -- we don't know lambda yet, but it may still be a scalar mult.
    f _  0  = No      -- Not a scalar multiple
    f ui vi = Yes $ ui / vi -- can still be a scalar multiple

    g No      = Nothing
    g Maybe   = error "scalarMultiple': found a Maybe, which means the vectors either have length zero, or one of them is all Zero!"
    g (Yes x) = Just x


--------------------------------------------------------------------------------
-- * unboxed vectors

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s (Vector d r) = MV_VectorD (UMV.MVector s r)
newtype instance UV.Vector     (Vector d r) = V_VectorD  (UV.Vector     r)

natVal' :: forall d. KnownNat d => Int
natVal' = fromIntegral $ natVal (Proxy @d)

instance ( GMV.MVector UMV.MVector r
         , Vector_ (Vector d r) d r
         ) => GMV.MVector UMV.MVector (Vector d r) where

  basicLength (MV_VectorD v) = let d = natVal' @d
                               in GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VectorD v) = let d = natVal' @d
                                        in MV_VectorD $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VectorD v) (MV_VectorD v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = let d = natVal' @d
                     in MV_VectorD <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VectorD v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VectorD v) i = let d = natVal' @d
                                     in generateA $ \j -> GMV.basicUnsafeRead v (d*i+j)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VectorD v) i w = imapMOf_ components f w
    where
      f j x = GMV.basicUnsafeWrite v (d*i+j) x
      d = natVal' @d
  {-# INLINE basicUnsafeWrite #-}


instance ( GV.Vector UV.Vector r
         , Vector_ (Vector d r) d r
         ) => GV.Vector UV.Vector (Vector d r) where

  basicUnsafeFreeze (MV_VectorD mv) = V_VectorD <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VectorD v) = MV_VectorD <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VectorD v) = let d = natVal' @d
                              in GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VectorD v) = let d = natVal' @d
                                       in V_VectorD $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VectorD v) i = let d = natVal' @d
                                      in generateA $ \j -> GV.basicUnsafeIndexM v (d*i+j)
  {-# INLINE basicUnsafeIndexM #-}

instance ( UV.Unbox r, Vector_ (Vector d r) d r
         ) => UV.Unbox (Vector d r) where
