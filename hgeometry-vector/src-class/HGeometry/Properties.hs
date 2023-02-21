{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Properties
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines some generic geometric properties e.g. Dimensions, NumType, and
-- Intersection types.
--
--------------------------------------------------------------------------------
module HGeometry.Properties(
    -- module Data.Intersection
   Dimension
  , NumType
  ) where

-- import Data.Intersection
import Data.Kind
-- import Data.Range
import GHC.TypeLits
-- import Data.Ext
-------------------------------------------------------------------------------

-- | A type family for types that are associated with a dimension. The
-- dimension is the dimension of the geometry they are embedded in.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: Type

--------------------------------------------------------------------------------

-- type instance NumType   (core :+ extra) = NumType core
-- type instance Dimension (core :+ extra) = Dimension core

type instance NumType [t] = NumType t
-- type instance NumType (Range a) = a
