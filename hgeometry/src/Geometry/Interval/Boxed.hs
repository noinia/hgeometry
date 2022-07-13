--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Interval.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Geometry.Interval.Boxed
  ( Interval(..)

  ) where

import Control.Lens
import Geometry.Interval.EndPoint
import Geometry.Interval.Class
import Geometry.Properties


--------------------------------------------------------------------------------

-- | Data type representing intervals
data Interval endPoint r = Interval !(endPoint r) !(endPoint r)
  deriving (Show,Eq,Ord)

type instance NumType (Interval endPoint r) = r

instance Functor endPoint => Functor (Interval endPoint) where
  fmap f (Interval s t) = Interval (fmap f s) (fmap f t)
instance Foldable endPoint => Foldable (Interval endPoint) where
  foldMap f (Interval s t) = foldMap f s <> foldMap f t
instance Traversable endPoint => Traversable (Interval endPoint) where
  traverse f (Interval s t) = Interval <$> traverse f s <*> traverse f t

instance HasStart (Interval endPoint r) (endPoint r) where
  start = lens (\(Interval s _) -> s) (\(Interval _ t) s -> Interval s t)

instance HasEnd (Interval endPoint r) (endPoint r) where
  end   = lens (\(Interval _ t) -> t) (\(Interval s _) t -> Interval s t)

instance EndPoint_ endPoint => Interval_ Interval endPoint r where
  mkInterval = Interval
