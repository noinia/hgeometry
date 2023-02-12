module V0 where

import GHC.Generics (Generic)

data Vec = Nil
  deriving stock (Eq,Ord,Generic)
