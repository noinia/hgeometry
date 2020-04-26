module Data.List.Alternating where

import Control.Lens
import Data.Bifoldable
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


-- | Computes a b with all its neighbours
--
-- >>> withNeighbours (Alternating 0 ['a' :+ 1, 'b' :+ 2, 'c' :+ 3])
-- [(0,'a' :+ 1),(1,'b' :+ 2),(2,'c' :+ 3)]
withNeighbours                     :: Alternating a b -> [(a,b :+ a)]
withNeighbours (Alternating a0 xs) = let as = a0 : map (^.extra) xs
                                     in zipWith (\a ba -> (a,ba)) as xs
