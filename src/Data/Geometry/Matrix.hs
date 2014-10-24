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

import Data.Vinyl.TypeLevel hiding (Nat)

--------------------------------------------------------------------------------

-- | a matrix of n rows, each of m columns, storing values of type r
newtype Matrix n m r = Matrix (Vector n (Vector m r))

-- deriving instance (Show r, Arity n, Arity m) => Show (Matrix n m r)
-- deriving instance (Eq r, Arity n, Arity m)   => Eq (Matrix n m r)
-- deriving instance (Ord r, Arity n, Arity m)  => Ord (Matrix n m r)
-- deriving instance (Arity n, Arity m)         => Functor (Matrix n m)


newtype Transformation d r = Transformation (Matrix (d + 1) (d + 1) r)

-- deriving instance (Show r, Arity d) => Show (Transformation d r)
-- deriving instance (Eq r, Arity d)   => Eq (Transformation d r)
-- deriving instance (Ord r, Arity d)  => Ord (Transformation d r)
-- deriving instance Arity d           => Functor (Transformation d)


class Transformable t where
  transform :: t -> Transformation (Dimension t) (NumType t) -> t

-- instance Num r => Transformable (Point d r) where
--   transform (Point v) (Transformation m) = Point . init $ m !* v'
--     where
--       v' = undefined -- extend with one 0 at the end
--       init = undefined -- drop the last value
