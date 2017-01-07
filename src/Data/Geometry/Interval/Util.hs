{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Interval.Util where

import Data.Range
import Control.Lens

-- | Open on left endpoint; so Closed before open
newtype L r = L { _unL :: EndPoint r } deriving (Show,Eq)
makeLenses ''L
instance Ord r => Ord (L r) where
  a `compare` b = f ( _unL a) `compare` f (_unL b)
    where
      f (Open x)   = (x,True)
      f (Closed x) = (x,False)

-- | Order on right endpoint; so Open before Closed
newtype R r = R { _unR :: EndPoint r } deriving (Show,Eq,Ord)
makeLenses ''R
