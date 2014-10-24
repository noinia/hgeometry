{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Geometry.Point where

import           Control.Lens(makeLenses)

-- import Data.TypeLevel.Common
import           Data.Typeable
import           Data.Vinyl hiding (Nat)
import qualified Data.Vinyl as V
import           Data.Vinyl.Functor(Const(..))
import qualified Data.Vinyl.TypeLevel as TV

import Data.Vinyl.TypeLevel hiding (Nat)

-- import qualified Data.Vector.Fixed as FV
-- import qualified Data.Vector.Fixed.Cont as C


import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           GHC.TypeLits

import           Linear.Vector(Additive(..))
import           Linear.Affine hiding (Point(..))
import           Linear.Matrix

--------------------------------------------------------------------------------

newtype Point d r = Point { toVec :: Vector d r }

type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d

instance RecApplicative (DimRange d) =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v

-- | a matrix of n rows, each of m columns, storing values of type r
newtype Matrix n m r = Matrix (Vector n (Vector m r))
                       -- deriving (Show,Read,Eq,Ord)


newtype Transformation d r = Transformation (Matrix (d + 1) (d + 1) r)
                             -- deriving (Show,Read,Eq,Ord)


class Transformable t where
  transform :: t -> Transformation (Dimension t) (NumType t) -> t

instance Num r => Transformable (Point d r) where
  transform (Point v) (Transformation m) = Point . init $ m !* v'
    where
      v' = undefined -- extend with one 0 at the end
      init = undefined -- drop the last value
