--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Matrix
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- type-indexed matrices.
--
--------------------------------------------------------------------------------
module Data.Geometry.Matrix(
    Matrix(Matrix)
  , identityMatrix

  , multM
  , mult

  , Invertible(..)
  , HasDeterminant(..)
  ) where

import           Control.Lens (imap)
import           Data.Geometry.Matrix.Internal (mkRow)
import           Data.Geometry.Vector
import           Linear.Matrix ((!*),(!*!))
import qualified Linear.Matrix as Lin
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- * Matrices

-- | a matrix of n rows, each of m columns, storing values of type r
newtype Matrix n m r = Matrix (Vector n (Vector m r))

deriving instance (Show r, Arity n, Arity m) => Show (Matrix n m r)
deriving instance (Eq r, Arity n, Arity m)   => Eq (Matrix n m r)
deriving instance (Ord r, Arity n, Arity m)  => Ord (Matrix n m r)
deriving instance (Arity n, Arity m)         => Functor (Matrix n m)
deriving instance (Arity n, Arity m)         => Foldable (Matrix n m)
deriving instance (Arity n, Arity m)         => Traversable (Matrix n m)

multM :: (Arity r, Arity c, Arity c', Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
(Matrix a) `multM` (Matrix b) = Matrix $ a !*! b

mult :: (Arity m, Arity n, Num r) => Matrix n m r -> Vector m r -> Vector n r
(Matrix m) `mult` v = m !* v

-- | Produces the Identity Matrix
identityMatrix :: (Arity d, Num r) => Matrix d d r
identityMatrix = Matrix $ imap mkRow (pure 1)

class Invertible n r where
  inverse' :: Matrix n n r -> Matrix n n r

instance Fractional r => Invertible 2 r where
  -- >>> inverse' $ Matrix $ Vector2 (Vector2 1 2) (Vector2 3 4.0)
  -- Matrix Vector2 [Vector2 [-2.0,1.0],Vector2 [1.5,-0.5]]
  inverse' (Matrix m) = Matrix . unsafeCoerce . Lin.inv22 . unsafeCoerce $ m

instance Fractional r => Invertible 3 r where
  -- >>> inverse' $ Matrix $ Vector3 (Vector3 1 2 4) (Vector3 4 2 2) (Vector3 1 1 1.0)
  -- Matrix Vector3 [Vector3 [0.0,0.5,-1.0],Vector3 [-0.5,-0.75,3.5],Vector3 [0.5,0.25,-1.5]]
  inverse' (Matrix m) = Matrix . unsafeCoerce . Lin.inv33 . unsafeCoerce $ m

instance Fractional r => Invertible 4 r where
  inverse' (Matrix m) = Matrix . unsafeCoerce . Lin.inv44 . unsafeCoerce $ m


class Arity d => HasDeterminant d where
  det :: Num r => Matrix d d r -> r

instance HasDeterminant 1 where
  det (Matrix (Vector1 (Vector1 x))) = x
instance HasDeterminant 2 where
  det = Lin.det22 . unsafeCoerce
instance HasDeterminant 3 where
  det = Lin.det33 . unsafeCoerce
instance HasDeterminant 4 where
  det = Lin.det44 . unsafeCoerce
