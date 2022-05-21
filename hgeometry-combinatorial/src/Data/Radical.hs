{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Radical
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Types that support computing Square roots
--------------------------------------------------------------------------------
module Data.Radical
  ( Radical(..)
  ) where

import           Data.Double.Approximate
import           Data.Double.Shaman
import qualified Prelude
import           Prelude hiding (sqrt)

--------------------------------------------------------------------------------

-- | Types that support taking a square root.
class Radical r where
  -- | Computes the square root of the number
  sqrt :: r -> r
  default sqrt :: Floating r => r -> r
  sqrt = Prelude.sqrt

instance Radical Float
instance Radical Double
instance Radical (DoubleRelAbs abs rel)
instance Radical (SDouble n)
instance Radical Shaman
