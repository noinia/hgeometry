module Algorithms.Geometry.SoS.Index
  ( HasSoSIndex(..)
  , SoSIndex
  , WithSoS(..)
  ) where

import Data.Ord
import Data.Geometry.Point

--------------------------------------------------------------------------------

type SoSIndex = Int

class HasSoSIndex a where
  sosIndex :: a -> SoSIndex


data WithSoS a = WithSoS {-# UNPACK #-} !SoSIndex a
               deriving (Show)


instance HasSoSIndex (WithSoS a) where
  sosIndex (WithSoS i _) = i
  {-# INLINE sosIndex #-}

instance Eq a => Eq (WithSoS a) where
  (WithSoS i x) == (WithSoS j y) = x == y && i == j

instance Ord a => Ord (WithSoS a) where
  (WithSoS i x) `compare` (WithSoS j y) = x `compare` y <> Down i `compare` Down j
