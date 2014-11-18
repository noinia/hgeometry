{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Transformation where


import           Control.Applicative
import           Control.Lens(lens,Lens',set)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Data.Proxy

import           GHC.TypeLits
import           Linear.Matrix((!*),(!*!))
import           Linear.Vector(zero)

import qualified Data.Vector.Fixed as FV
import qualified Data.Geometry.Vector as V

import           Data.Vinyl.TypeLevel hiding (Nat)

--------------------------------------------------------------------------------

-- | a matrix of n rows, each of m columns, storing values of type r
newtype Matrix n m r = Matrix (Vector n (Vector m r))

deriving instance (Show r, Arity n, Arity m) => Show (Matrix n m r)
deriving instance (Eq r, Arity n, Arity m)   => Eq (Matrix n m r)
deriving instance (Ord r, Arity n, Arity m)  => Ord (Matrix n m r)
deriving instance (Arity n, Arity m)         => Functor (Matrix n m)

multM :: (Arity r, Arity c, Arity c', Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
(Matrix a) `multM` (Matrix b) = Matrix $ a !*! b

mult :: (Arity m, Arity n, Num r) => Matrix n m r -> Vector m r -> Vector n r
(Matrix m) `mult` v = m !* v

--------------------------------------------------------------------------------

-- | A type representing a Transformation for d dimensional objects
newtype Transformation d r = Transformation { _transformationMatrix :: Matrix (1 + d) (1 + d) r }

transformationMatrix :: Lens' (Transformation d r) (Matrix (1 + d) (1 + d) r)
transformationMatrix = lens _transformationMatrix (const Transformation)

deriving instance (Show r, Arity (1 + d)) => Show (Transformation d r)
deriving instance (Eq r, Arity (1 + d))   => Eq (Transformation d r)
deriving instance (Ord r, Arity (1 + d))  => Ord (Transformation d r)
deriving instance Arity (1 + d)           => Functor (Transformation d)

type instance NumType (Transformation d r) = r


-- | Compose transformations (right to left)
(|.|) :: (Num r, Arity (1 + d)) => Transformation d r -> Transformation d r -> Transformation d r
(Transformation f) |.| (Transformation g) = Transformation $ f `multM` g

--------------------------------------------------------------------------------

-- | A class representing types that can be transformed using a transformation
class IsTransformable g where
  transformBy :: Transformation (Dimension g) (NumType g) -> g -> g

transformAllBy :: (Functor c, IsTransformable g)
               => Transformation (Dimension g) (NumType g) -> c g -> c g
transformAllBy t = fmap (transformBy t)


type AlwaysTruePFT d = AlwaysTrueDestruct d  (1 + d)


transformPointFunctor   :: ( PointFunctor g, Num r, d ~ Dimension (g r)
                           , AlwaysTruePFT d
                           ) => Transformation d r -> g r -> g r
transformPointFunctor t = pmap (transformBy t)

instance ( Num r
         , Arity d, AlwaysTrueDestruct d (1 + d)
         ) => IsTransformable (Point d r) where
  transformBy (Transformation m) (Point v) = Point . V.init $ m `mult` v'
    where
      v'    = snoc v 0


--------------------------------------------------------------------------------
-- | Common transformations

translation   :: ( Num r, Arity (1 + d)
                 , AlwaysTrueSnoc d, Arity d, Index' (1+d-1) (1+d))
              => Vector d r -> Transformation d r
translation v = Transformation . Matrix $ V.imap transRow (snoc v 1)


scaling   :: (Num r, Arity (1 + d), AlwaysTrueSnoc d, Arity d) => Vector d r -> Transformation d r
scaling v = Transformation . Matrix $ V.imap mkRow (snoc v 1)

uniformScaling :: (Num r, Arity (1 + d), AlwaysTrueSnoc d, Arity d) => r -> Transformation d r
uniformScaling = scaling . pure

--------------------------------------------------------------------------------
-- | Functions that execute transformations

type AlwaysTrueTransformation d = (Arity (1 + d), AlwaysTrueSnoc d, Arity d, Index' (1+d-1) (1+d))

translateBy :: ( IsTransformable g, Num (NumType g)
               , AlwaysTrueTransformation (Dimension g)
               ) => Vector (Dimension g) (NumType g) -> g -> g
translateBy = transformBy . translation

scaleBy :: ( IsTransformable g, Num (NumType g)
           , AlwaysTrueTransformation (Dimension g)
           ) => Vector (Dimension g) (NumType g) -> g -> g
scaleBy = transformBy . scaling


scaleUniformlyBy :: ( IsTransformable g, Num (NumType g)
                    , AlwaysTrueTransformation (Dimension g)
                    ) => NumType g -> g -> g
scaleUniformlyBy = transformBy  . uniformScaling


--------------------------------------------------------------------------------
-- | Helper functions to easily create matrices

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Arity d, Num r) => Int -> r -> Vector d r
mkRow i x = set (FV.element i) x zero

-- | Row in a translation matrix
transRow     :: forall n r. (Arity n, Index' (n-1) n, Num r) => Int -> r -> Vector n r
transRow i x = set (V.element (Proxy :: Proxy (n-1))) x $ mkRow i 1
