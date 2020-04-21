{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Transformation where

import           Control.Lens (iso,Lens',set,Iso)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as V
import           Data.Proxy
import qualified Data.Vector.Fixed as FV
import           GHC.TypeLits
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
identityMatrix = Matrix $ V.imap mkRow (pure 1)

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

--------------------------------------------------------------------------------
-- * Transformations

-- | A type representing a Transformation for d dimensional objects
newtype Transformation d r = Transformation { _transformationMatrix :: Matrix (d + 1) (d + 1) r }

transformationMatrix :: Iso (Transformation d r)       (Transformation d       s)
                            (Matrix (d + 1) (d + 1) r) (Matrix (d + 1) (d + 1) s)
transformationMatrix = iso _transformationMatrix Transformation

deriving instance (Show r, Arity (d + 1)) => Show (Transformation d r)
deriving instance (Eq r, Arity (d + 1))   => Eq (Transformation d r)
deriving instance (Ord r, Arity (d + 1))  => Ord (Transformation d r)
deriving instance Arity (d + 1)           => Functor (Transformation d)
deriving instance Arity (d + 1)           => Foldable (Transformation d)
deriving instance Arity (d + 1)           => Traversable (Transformation d)

type instance NumType (Transformation d r) = r

-- | Compose transformations (right to left)
(|.|) :: (Num r, Arity (d + 1)) => Transformation d r -> Transformation d r -> Transformation d r
(Transformation f) |.| (Transformation g) = Transformation $ f `multM` g


-- if it exists?

-- | Compute the inverse transformation
--
-- >>> inverseOf $ translation (Vector2 (10.0) (5.0))
-- Transformation {_transformationMatrix = Matrix Vector3 [Vector3 [1.0,0.0,-10.0],Vector3 [0.0,1.0,-5.0],Vector3 [0.0,0.0,1.0]]}
inverseOf :: (Fractional r, Invertible (d + 1) r)
          => Transformation d r -> Transformation d r
inverseOf = Transformation . inverse' . _transformationMatrix

--------------------------------------------------------------------------------
-- * Transformable geometry objects

-- | A class representing types that can be transformed using a transformation
class IsTransformable g where
  transformBy :: Transformation (Dimension g) (NumType g) -> g -> g

transformAllBy :: (Functor c, IsTransformable g)
               => Transformation (Dimension g) (NumType g) -> c g -> c g
transformAllBy t = fmap (transformBy t)


transformPointFunctor   :: ( PointFunctor g, Fractional r, d ~ Dimension (g r)
                           , Arity d, Arity (d + 1)
                           ) => Transformation d r -> g r -> g r
transformPointFunctor t = pmap (transformBy t)

instance (Fractional r, Arity d, Arity (d + 1))
         => IsTransformable (Point d r) where
  transformBy t = Point . transformBy t . toVec

instance (Fractional r, Arity d, Arity (d + 1))
         => IsTransformable (Vector d r) where
  transformBy (Transformation m) v = f $ m `mult` snoc v 1
    where
      f u   = (/ V.last u) <$> V.init u


--------------------------------------------------------------------------------
-- * Common transformations

translation   :: (Num r, Arity d, Arity (d + 1))
              => Vector d r -> Transformation d r
translation v = Transformation . Matrix $ V.imap transRow (snoc v 1)


scaling   :: (Num r, Arity d, Arity (d + 1))
          => Vector d r -> Transformation d r
scaling v = Transformation . Matrix $ V.imap mkRow (snoc v 1)

uniformScaling :: (Num r, Arity d, Arity (d + 1)) => r -> Transformation d r
uniformScaling = scaling . pure

--------------------------------------------------------------------------------
-- * Functions that execute transformations

translateBy :: ( IsTransformable g, Num (NumType g)
               , Arity (Dimension g), Arity (Dimension g + 1)
               ) => Vector (Dimension g) (NumType g) -> g -> g
translateBy = transformBy . translation

scaleBy :: ( IsTransformable g, Num (NumType g)
           , Arity (Dimension g), Arity (Dimension g + 1)
           ) => Vector (Dimension g) (NumType g) -> g -> g
scaleBy = transformBy . scaling


scaleUniformlyBy :: ( IsTransformable g, Num (NumType g)
                    , Arity (Dimension g), Arity (Dimension g + 1)
                    ) => NumType g -> g -> g
scaleUniformlyBy = transformBy  . uniformScaling


--------------------------------------------------------------------------------
-- * Helper functions to easily create matrices

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Arity d, Num r) => Int -> r -> Vector d r
mkRow i x = set (FV.element i) x zero

-- | Row in a translation matrix
-- transRow     :: forall n r. ( Arity n, Arity (n- 1), ((n - 1) + 1) ~ n
--                             , Num r) => Int -> r -> Vector n r
-- transRow i x = set (V.element (Proxy :: Proxy (n-1))) x $ mkRow i 1

transRow     :: forall n r. (Arity n, Arity (n + 1), Num r)
             => Int -> r -> Vector (n + 1) r
transRow i x = set (V.element (Proxy :: Proxy n)) x $ mkRow i 1

--------------------------------------------------------------------------------
-- * 3D Rotations

-- | Given three new unit-length basis vectors (u,v,w) that map to (x,y,z),
-- construct the appropriate rotation that does this.
--
--
rotateTo                 :: Num r => Vector 3 (Vector 3 r) -> Transformation 3 r
rotateTo (Vector3 u v w) = Transformation . Matrix $ Vector4 (snoc u        0)
                                                             (snoc v        0)
                                                             (snoc w        0)
                                                             (Vector4 0 0 0 1)
