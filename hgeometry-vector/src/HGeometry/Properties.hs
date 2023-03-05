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
    -- module HGeometry.Intersection
   Dimension
  , NumType
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
-- import HGeometry.Intersection
-- import Data.Range
import GHC.TypeNats
import HGeometry.Ext
-------------------------------------------------------------------------------

-- | A type family for types that are associated with a dimension. The
-- dimension is the dimension of the geometry they are embedded in.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: Type

--------------------------------------------------------------------------------

type instance NumType   (core :+ extra) = NumType core
type instance Dimension (core :+ extra) = Dimension core

type instance NumType   [t] = NumType t
type instance Dimension [t] = Dimension t
-- type instance NumType (Range a) = a

type instance Dimension (NonEmpty g) = Dimension g
type instance NumType   (NonEmpty g) = NumType g
