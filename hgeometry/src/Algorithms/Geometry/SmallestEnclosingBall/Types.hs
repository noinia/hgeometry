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
import           Data.Geometry
import           Data.Geometry.Ball
import           Control.Lens
import           Data.Ext

--------------------------------------------------------------------------------

-- | List of two or three elements
data TwoOrThree a = Two !a !a | Three !a !a !a deriving (Show,Read,Eq,Ord,Functor)

instance F.Foldable TwoOrThree where
  foldMap f (Two   a b)   = f a <> f b
  foldMap f (Three a b c) = f a <> f b <> f c


fromList         :: [a] -> Either String (TwoOrThree a)
fromList [a,b]   = Right $ Two a b
fromList [a,b,c] = Right $ Three a b c
fromList _       = Left "Wrong number of elements"




-- | The result of a smallest enclosing disk computation: The smallest ball
--    and the points defining it
data DiskResult p r = DiskResult { _enclosingDisk  :: Disk () r
                                 , _definingPoints :: TwoOrThree (Point 2 r :+ p)
                                 } deriving (Show,Eq)
makeLenses ''DiskResult
