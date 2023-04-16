module HGeometry.Measured.Count
  ( Count(..)
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Maintains the count
newtype Count = Count { getCount :: Word }
              deriving stock (Show,Eq,Ord,Generic)
              deriving newtype (Num,Integral,Enum,Real,NFData)

instance Semigroup Count where
  a <> b = Count $ getCount a + getCount b
instance Monoid Count where
  mempty = 0
  mappend = (<>)
