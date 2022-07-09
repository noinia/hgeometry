{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SmallestEnclosingBall.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Types to represent the smallest enclosing disk of a set of points in
-- \(\mathbb{R}^2\)
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.SmallestEnclosingBall.Types where

import qualified Data.Foldable as F
import           Geometry.Ball
import           Control.Lens

--------------------------------------------------------------------------------

-- | List of two or three elements
data TwoOrThree a = Two !a !a | Three !a !a !a deriving (Show,Read,Eq,Ord,Functor)

instance F.Foldable TwoOrThree where
  foldMap f (Two   a b)   = f a <> f b
  foldMap f (Three a b c) = f a <> f b <> f c

-- | Construct datatype from list with exactly two or three elements.
twoOrThreeFromList         :: [a] -> Either String (TwoOrThree a)
twoOrThreeFromList [a,b]   = Right $ Two a b
twoOrThreeFromList [a,b,c] = Right $ Three a b c
twoOrThreeFromList _       = Left "Wrong number of elements"




-- | The result of a smallest enclosing disk computation: The smallest ball
--    and the points defining it
data DiskResult point r = DiskResult { _enclosingDisk  :: Disk () r
                                     , _definingPoints :: TwoOrThree (point 2 r)
                                     }

deriving instance (Show r, Show (point 2 r)) => Show (DiskResult point r)
deriving instance (Eq r, Eq (point 2 r))     => Eq   (DiskResult point r)

makeLenses ''DiskResult
