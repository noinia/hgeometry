--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Trie
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Trie type
--
--------------------------------------------------------------------------------
module HGeometry.Trie
  ( TrieF(..)

  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable1
import Data.Functor.Apply ((<.*>))
import Data.Functor.Classes
import Data.Monoid (Endo(..))
import Data.Semigroup.Traversable

--------------------------------------------------------------------------------

-- | The Trie data type, parameterized by the data structure storing the children.
data TrieF f e v = Node v (f e (TrieF f e v))

deriving instance (Show v, Show e, Show2 f) => Show (TrieF f e v)
deriving instance (Eq v, Eq e, Eq2 f)       => Eq (TrieF f e v)
deriving instance (Ord v, Ord e, Ord2 f)    => Ord (TrieF f e v)

deriving instance (Functor (f e))     => Functor (TrieF f e)
deriving instance (Foldable (f e))    => Foldable (TrieF f e)
deriving instance (Traversable (f e)) => Traversable (TrieF f e)

instance Foldable (f e) => Foldable1 (TrieF f e) where
  foldMap1 f (Node v chs) = let Endo g = foldMap (\x -> Endo $ \x0 -> x0 <> foldMap1 f x) chs
                            in g (f v)

instance Traversable (f e) => Traversable1 (TrieF f e) where
  traverse1 f (Node v chs) =
    Node <$> f v <.*> traverse1Maybe (traverse1 f) chs


instance Bifunctor f => Bifunctor (TrieF f) where
  bimap f g (Node v chs) = Node (g v) (bimap f (bimap f g) chs)

instance Bifoldable f => Bifoldable (TrieF f) where
  bifoldMap f g (Node v chs) = g v <> bifoldMap f (bifoldMap f g) chs

instance Bitraversable f => Bitraversable (TrieF f) where
  bitraverse f g (Node v chs) = Node <$> g v <*> bitraverse f (bitraverse f g) chs
