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

import           Data.Geometry.Vector
import           GHC.TypeLits

import           Linear.Vector(Additive(..))
import           Linear.Affine hiding (Point(..))

--------------------------------------------------------------------------------

newtype Point d r = Point { toVec :: Vector d r }

instance RecApplicative (DimRange d) =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v
