{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Matrix
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- type-indexed matrices.
--
--------------------------------------------------------------------------------
module HGeometry.Matrix(
    Matrix(Matrix)
  , identityMatrix

  , multM
  , mult

  , Invertible(..)
  , HasDeterminant(..)
  ) where

import           Control.Lens
import           Data.Coerce
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           HGeometry.Matrix.Class
import           HGeometry.Matrix.Internal (mkRow)
import           HGeometry.Properties
import           HGeometry.Vector
import           Linear.Matrix (M22, M33, M44, (!*!), (!*))
import qualified Linear.Matrix as Lin

--------------------------------------------------------------------------------
-- * Matrices

-- | A matrix with n rows and m coloumns, each element a value of type r.
type Matrix :: Nat -> Nat -> Type -> Type
newtype Matrix n m r = Matrix (Vector (n GHC.TypeLits.* m) r)
  -- the matrix is stored column by column

type instance NumType (Matrix n m r) = r
type instance IxValue (Matrix n m r) = r
type instance Index   (Matrix n m r) = (Int,Int) -- | (row,column)

deriving instance Show (Vector (n GHC.TypeLits.* m) r) => Show (Matrix n m r)

-- columns   :: Matrix n m r -> Vector n (Vector m r)
-- columns m = undefined

-- rows :: Matrix n m r -> Vector m (Vector n r)
-- rows = columns . transpose

instance ( KnownNat m
         , Ixed (Vector (n GHC.TypeLits.* m) r)
         ) => Ixed (Matrix n m r) where
  ix (r,c) f (Matrix v) = let m = fromInteger $ natVal $ Proxy @m
                          in Matrix <$> ix ((r*m)+c) f v


instance ( KnownNat n, KnownNat m
         , Vector_ (Vector (n GHC.TypeLits.* m) r) (n GHC.TypeLits.* m) r
         ) => Matrix_ (Matrix n m r) n m r where
  identityMatrix = let m = fromInteger $ natVal $ Proxy @m
                   in Matrix <$> generate $ \i -> let (a,b) = quotRem i m
                                                  in if a == b then 1 else 0

  multM = undefined

  mult = undefined


-- -- deriving instance (Show r, Arity n, Arity m) => Show (Matrix n m r)
-- -- deriving instance (Eq r, Arity n, Arity m)   => Eq (Matrix n m r)
-- -- deriving instance (Ord r, Arity n, Arity m)  => Ord (Matrix n m r)
-- -- deriving instance (Arity n, Arity m)         => Functor (Matrix n m)
-- -- deriving instance (Arity n, Arity m)         => Foldable (Matrix n m)
-- -- deriving instance (Arity n, Arity m)         => Traversable (Matrix n m)

-- -- | Matrix product.
-- multM :: (Arity r, Arity c, Arity c', Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
-- (Matrix a) `multM` (Matrix b) = Matrix $ a !*! b

-- -- | Matrix * column vector.
-- mult :: (Arity m, Arity n, Num r) => Matrix n m r -> Vector m r -> Vector n r
-- (Matrix m) `mult` v = m !* v

-- -- | Produces the Identity Matrix.
-- identityMatrix :: (Arity d, Num r) => Matrix d d r
-- identityMatrix = Matrix $ imap mkRow (pure 1)

-- -- | Class of matrices that are invertible.
-- class Invertible n r where
--   inverse' :: Matrix n n r -> Matrix n n r

-- instance Fractional r => Invertible 2 r where
--   -- >>> inverse' $ Matrix $ Vector2 (Vector2 1 2) (Vector2 3 4.0)
--   -- Matrix Vector2 [Vector2 [-2.0,1.0],Vector2 [1.5,-0.5]]
--   inverse' = withM22 Lin.inv22

-- instance Fractional r => Invertible 3 r where
--   -- >>> inverse' $ Matrix $ Vector3 (Vector3 1 2 4) (Vector3 4 2 2) (Vector3 1 1 1.0)
--   -- Matrix Vector3 [Vector3 [0.0,0.5,-1.0],Vector3 [-0.5,-0.75,3.5],Vector3 [0.5,0.25,-1.5]]
--   inverse' = withM33 Lin.inv33

-- instance Fractional r => Invertible 4 r where
--   inverse' = withM44 Lin.inv44

-- -- | Class of matrices that have a determinant.
-- class Arity d => HasDeterminant d where
--   det :: Num r => Matrix d d r -> r

-- instance HasDeterminant 1 where
--   det (Matrix (Vector1 (Vector1 x))) = x
-- instance HasDeterminant 2 where
--   det = Lin.det22 . coerce
-- instance HasDeterminant 3 where
--   det = Lin.det33 . coerce
-- instance HasDeterminant 4 where
--   det = Lin.det44 . coerce

-- --------------------------------------------------------------------------------
-- -- Boilerplate code for converting between Matrix and M22/M33/M44.

-- withM22 :: (M22 a -> M22 b) -> Matrix 2 2 a -> Matrix 2 2 b
-- withM22 f = coerce . f . coerce

-- withM33 :: (M33 a -> M33 b) -> Matrix 3 3 a -> Matrix 3 3 b
-- withM33 f = coerce . f . coerce

-- withM44 :: (M44 a -> M44 b) -> Matrix 4 4 a -> Matrix 4 4 b
-- withM44 f = coerce . f . coerce
