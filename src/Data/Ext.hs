module Data.Ext where

import Control.Lens

--------------------------------------------------------------------------------

data core :+ extra = core :+ extra deriving (Show,Read,Eq,Ord)
infixr 1 :+

_core :: (core :+ extra) -> core
_core (c :+ _) = c

_extra :: (core :+ extra) -> extra
_extra (_ :+ e) = e

core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> (c :+ e))

extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> (c :+ e))
