module Data.Ext where

import Control.Applicative
import Control.Lens
import Data.Semigroup

--------------------------------------------------------------------------------

data Ext extra core = core :+ extra deriving (Show,Read,Eq,Ord)

instance Functor (Ext e) where
  fmap f (c :+ e) = f c :+ e

instance Monoid e => Applicative (Ext e) where
  pure x = x :+ mempty
  -- | This implementation ignores any extra values f may have
  (f :+ _) <*> (c :+ce) = f c :+ ce

instance (Semigroup core, Semigroup extra) => Semigroup (Ext extra core) where
  (c :+ e) <> (c' :+ e') = c <> c' :+ e <> e'


type core :+ extra = Ext extra core
infixr 1 :+

_core :: (core :+ extra) -> core
_core (c :+ _) = c

_extra :: (core :+ extra) -> extra
_extra (_ :+ e) = e

core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> (c :+ e))

extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> (c :+ e))
