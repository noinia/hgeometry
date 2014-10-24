{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Point where

import           Control.Lens(makeLenses)

-- import Data.TypeLevel.Common
import           Data.Typeable
import           Data.Vinyl hiding (Nat)
import qualified Data.Vinyl as V
import           Data.Vinyl.Functor(Const(..))
import qualified Data.Vinyl.TypeLevel as TV

import Data.Vinyl.TypeLevel hiding (Nat)

import qualified Data.Vector.Fixed as FV
-- import qualified Data.Vector.Fixed.Cont as C


import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           GHC.TypeLits

import           Linear.Vector(Additive(..))
import           Linear.Affine hiding (Point(..))
import           Linear.Matrix

--------------------------------------------------------------------------------

newtype Point d r = Point { toVec :: Vector d r }

deriving instance (Show r, Arity d) => Show (Point d r)
deriving instance (Eq r, Arity d)   => Eq (Point d r)
deriving instance (Ord r, Arity d)  => Ord (Point d r)
deriving instance Arity d           => Functor (Point d)


type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d



instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v




unsafeCoord             :: Arity d => Point d r -> Int -> r
unsafeCoord (Point v) i = v FV.! i


pattern Point2 x y <- (_point2 -> (x,y))

point2 x y = Point . Vector $ FV.mk2 x y

_point2 p = (unsafeCoord p 1, unsafeCoord p 2)
