{-# LANGUAGE TypeFamilyDependencies #-}
module HGeometry.Vector.Optimal.Defs
  ( ByDimension(..)
  , Choose
  ) where

import           GHC.TypeLits

--------------------------------------------------------------------------------

-- | Small vectors have a specific representation
data ByDimension d = Zero | One | Two | Three | Four | Large d

-- | Type family to choose a particular representation based on dimension.
type Choose :: Nat -> ByDimension Nat
type family Choose d = result | result -> d where
  Choose 0 = Zero
  Choose 1 = One
  Choose 2 = Two
  Choose 3 = Three
  Choose 4 = Four
  Choose d = Large d
