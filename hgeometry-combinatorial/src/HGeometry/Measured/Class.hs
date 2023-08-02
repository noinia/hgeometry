{-# Language FunctionalDependencies #-}
{-# Language DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Measured.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Measured.Class
  ( Measured(..)
  , CanInsert(..)
  , CanDelete(..)
  ) where

-- | Things that can be measured.
class Measured f a where
  -- | Given a single a, measure it into someting of type 'f a'.
  measure :: a -> f a

-- | Things that can be inserted.
class Measured f a => CanInsert f a where
  {-# MINIMAL #-}
  -- | Delete an element form the associated structure
  insertMeasure :: a -> f a -> f a
  default insertMeasure :: (Semigroup (f a)) => a -> f a -> f a
  insertMeasure x m = measure x <> m

-- | Things that can be deleted.
class Measured f a => CanDelete f a where
  -- | Delete the element from the structure. Returns a Maybe since the structure may be
  -- empty after deletion (and that may not be representable by f).
  deleteMeasure :: a -> f a -> Maybe (f a)
