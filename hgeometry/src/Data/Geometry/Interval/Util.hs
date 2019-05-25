{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Interval.Util where

import Control.DeepSeq
import Control.Lens
import Data.Range
import GHC.Generics (Generic)


-- | Open on left endpoint; so Closed before open
newtype L r = L { _unL :: EndPoint r } deriving (Show,Eq,Generic,NFData)
makeLenses ''L
instance Ord r => Ord (L r) where
  a `compare` b = f ( _unL a) `compare` f (_unL b)
    where
      f (Open x)   = (x,True)
      f (Closed x) = (x,False)

-- | Order on right endpoint; so Open before Closed
newtype R r = R { _unR :: EndPoint r } deriving (Show,Eq,Ord,Generic,NFData)
makeLenses ''R
