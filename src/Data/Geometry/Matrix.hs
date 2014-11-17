{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Matrix where

import           Control.Lens(set)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Data.Proxy

import           GHC.TypeLits
import           Linear.Matrix((!*))
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


newtype Transformation d r = Transformation (Matrix (1 + d) (1 + d) r)

deriving instance (Show r, Arity (1 + d)) => Show (Transformation d r)
deriving instance (Eq r, Arity (1 + d))   => Eq (Transformation d r)
deriving instance (Ord r, Arity (1 + d))  => Ord (Transformation d r)
deriving instance Arity (1 + d)           => Functor (Transformation d)


mult :: (Arity m, Arity n, Num r) => Matrix n m r -> Vector m r -> Vector n r
(Matrix m) `mult` v = m !* v


class Transformable t where
  transformBy :: Transformation (Dimension t) (NumType t) -> t -> t

instance ( Num r
         , Arity d, AlwaysTrueDestruct d (1 + d)
         ) => Transformable (Point d r) where
  transformBy (Transformation m) (Point v) = Point . V.init $ m `mult` v'
    where
      v'    = snoc v 0


-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Arity d, Num r) => Int -> r -> Vector d r
mkRow i x = set (FV.element i) x zero

-- | Row in a translation matrix
transRow     :: forall n r. (Arity n, Index' n n, Num r) => Int -> r -> Vector n r
transRow i x = set (V.element (Proxy :: Proxy n)) x $ mkRow i 1


translation   :: (Num r, Arity d, Index' (1+d) (1+d), Arity (1 + d))
              => Vector d r -> Transformation d r
translation v = Transformation . Matrix $ V.imap transRow (snoc v 1)


scaling   :: (Num r, Arity d, Arity (1 + d)) => Vector d r -> Transformation d r
scaling v = Transformation . Matrix $ V.imap mkRow (snoc v 1)


t2 :: Transformation 2 Double
t2 = scaling . Vector $ FV.mk2 2 2
