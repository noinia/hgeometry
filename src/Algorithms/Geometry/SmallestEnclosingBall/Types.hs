{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Algorithms.Geometry.SmallestEnclosingBall.Types where

import           Data.Monoid
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

-- | The result of a smallest enclosing disk computation: The smallest ball
--    and the points defining it
data DiskResult p r = DiskResult { _enclosingDisk  :: Disk () r
                                 , _definingPoints :: TwoOrThree (Point 2 r :+ p)
                                 }
makeLenses ''DiskResult
