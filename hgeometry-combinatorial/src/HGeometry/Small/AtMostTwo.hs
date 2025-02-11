--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Small.AtMostTwo
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type expressing that some value may have at most two values
--
--------------------------------------------------------------------------------
module HGeometry.Small.AtMostTwo
  ( AtMostTwo(..)
  ) where

import           Data.Functor.Classes

--------------------------------------------------------------------------------

-- | At most two elements
data AtMostTwo a = Zero | One !a | Two !a !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Show1 AtMostTwo where
  liftShowsPrec sp _ d = \case
    Zero ->
      \s -> s <> "Zero"
    One x ->
      showsUnaryWith sp "One" d x
    Two x y ->
      showsBinaryWith sp sp "Two" d x y

instance Eq1   AtMostTwo where
  liftEq _ Zero      Zero        = True
  liftEq f (One x)   (One x')    = f x x'
  liftEq f (Two x y) (Two x' y') = f x x' && f y y'
  liftEq _ _         _           = False

instance Ord1  AtMostTwo where
  liftCompare _ Zero      Zero        = EQ
  liftCompare _ Zero      _           = LT

  liftCompare _ (One _)   Zero        = GT
  liftCompare f (One x)   (One x')    = f x x'
  liftCompare _ (One _)   (Two _ _)   = LT

  liftCompare f (Two x y) (Two x' y') = f x x' <> f y y'
  liftCompare _ _         _           = GT
