{-# Language FunctionalDependencies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Measured.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Measured.Class where

-- | Things that can be measured.
class Semigroup v => Measured v a | a -> v where
  measure :: a -> v

-- | Things that can be inserted.
class Measured v a => CanInsert v a where
  insertA :: a -> v -> v

-- | Things that can be deleted.
class Measured v a => CanDelete v a where
  deleteA :: a -> v -> Maybe v
