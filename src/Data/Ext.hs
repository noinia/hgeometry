module Data.Ext where

import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Biapplicative
import Data.Bifunctor.Apply
import Data.Bifoldable
import Data.Bitraversable
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Functor.Apply(liftF2)

--------------------------------------------------------------------------------

data core :+ extra = core :+ extra deriving (Show,Read,Eq,Ord)
infixr 1 :+

-- data Ext extra core = core :+ extra deriving (Show,Read,Eq,Ord)

instance Bifunctor (:+) where
  bimap f g (c :+ e) = f c :+ g e

instance Biapply (:+) where
  (f :+ g) <<.>> (c :+ e) = f c :+ g e

instance Biapplicative (:+) where
  bipure = (:+)
  (f :+ g) <<*>> (c :+ e) = f c :+ g e

instance Bifoldable (:+) where
  bifoldMap f g (c :+ e) = f c `mappend` g e

instance Bitraversable (:+) where
  bitraverse f g (c :+ e) = (:+) <$> f c <*> g e

instance Bifoldable1 (:+)

instance Bitraversable1 (:+) where
  bitraverse1 f g (c :+ e) = liftF2 (:+) (f c) (g e)

instance (Semigroup core, Semigroup extra) => Semigroup (core :+ extra) where
  (c :+ e) <> (c' :+ e') = c <> c' :+ e <> e'


_core :: (core :+ extra) -> core
_core (c :+ _) = c

_extra :: (core :+ extra) -> extra
_extra (_ :+ e) = e

core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> (c :+ e))

extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> (c :+ e))

ext   :: a -> a :+ ()
ext x = x :+ ()
