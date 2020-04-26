module Data.List.Alternating where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Ext

--------------------------------------------------------------------------------

-- | A (non-empty) alternating list of a's and b's
data Alternating a b = Alternating a [b :+ a] deriving (Show,Eq,Ord)

instance Bifunctor Alternating where
  bimap = bimapDefault
instance Bifoldable Alternating where
  bifoldMap = bifoldMapDefault
instance Bitraversable Alternating where
  bitraverse f g (Alternating a xs) = Alternating <$> f a <*> traverse (bitraverse g f) xs
