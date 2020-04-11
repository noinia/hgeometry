{-# Language FunctionalDependencies #-}
module Data.Measured.Class where

class Semigroup v => Measured v a | a -> v where
  measure :: a -> v

class Measured v a => CanInsert v a where
  insertA :: a -> v -> v

class Measured v a => CanDelete v a where
  deleteA :: a -> v -> Maybe v
