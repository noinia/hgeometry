{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Matrix where

import Data.Geometry.Vector
import Data.Geometry.Point
import Data.Geometry.Properties

import GHC.TypeLits
import           Linear.Matrix

import qualified Data.Geometry.Vector as V

import Data.Vinyl.TypeLevel hiding (Nat)

--------------------------------------------------------------------------------

-- | a matrix of n rows, each of m columns, storing values of type r
newtype Matrix n m r = Matrix (Vector n (Vector m r))

-- deriving instance (Show r, Arity n, Arity m) => Show (Matrix n m r)
-- deriving instance (Eq r, Arity n, Arity m)   => Eq (Matrix n m r)
-- deriving instance (Ord r, Arity n, Arity m)  => Ord (Matrix n m r)
-- deriving instance (Arity n, Arity m)         => Functor (Matrix n m)


newtype Transformation d r = Transformation (Matrix (1 + d) (1 + d) r)

-- deriving instance (Show r, Arity d) => Show (Transformation d r)
-- deriving instance (Eq r, Arity d)   => Eq (Transformation d r)
-- deriving instance (Ord r, Arity d)  => Ord (Transformation d r)
-- deriving instance Arity d           => Functor (Transformation d)


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
