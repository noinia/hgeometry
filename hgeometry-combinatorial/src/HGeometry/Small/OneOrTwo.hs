--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Small.OneOrTwo
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type expressing that some value may have one or two values
--
--------------------------------------------------------------------------------
module HGeometry.Small.OneOrTwo
  ( OneOrTwo(..)
  ) where

import Data.Semigroup.Traversable
import Data.Functor.Classes
import Data.Foldable1
import Data.Functor.Apply (liftF2)

--------------------------------------------------------------------------------

-- | At most two elements
data OneOrTwo a = One !a | Two !a !a
  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Show1 OneOrTwo where
  liftShowsPrec sp _ d = \case
    One x ->
      showsUnaryWith sp "One" d x
    Two x y ->
      showsBinaryWith sp sp "Two" d x y

instance Eq1   OneOrTwo where
  liftEq f (One x)   (One x')    = f x x'
  liftEq f (Two x y) (Two x' y') = f x x' && f y y'
  liftEq _ _         _           = False

instance Ord1  OneOrTwo where
  liftCompare f (One x)   (One x')    = f x x'
  liftCompare _ (One _)   (Two _ _)   = LT

  liftCompare f (Two x y) (Two x' y') = f x x' <> f y y'
  liftCompare _ _         _           = GT

instance Foldable1 OneOrTwo where
  foldMap1 f = \case
    One x   -> f x
    Two x y -> f x <> f y

instance Traversable1 OneOrTwo where
  traverse1 f = \case
    One x   -> One <$> f x
    Two x y -> liftF2 Two (f x) (f y)


--------------------------------------------------------------------------------
