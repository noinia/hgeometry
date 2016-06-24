module Data.Ext where

import Control.Lens
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Apply
import Data.Bitraversable
import Data.Functor.Apply (liftF2)
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable

--------------------------------------------------------------------------------

data core :+ extra = core :+ extra deriving (Show,Read,Eq,Ord,Bounded)
infixr 1 :+


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
