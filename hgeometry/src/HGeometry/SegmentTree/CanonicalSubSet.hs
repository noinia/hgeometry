module HGeometry.SegmentTree.CanonicalSubSet
  ( HasCanonicalSubSet(..)
  ) where

import Control.Lens

--------------------------------------------------------------------------------

-- | Types that store canonical subsets
class HasCanonicalSubSet s t a f g | s -> f
                                   , t -> g where
  -- | Lens to access the canonical subset of a node or leaf
  canonicalSubSet :: Lens s t (f a) (g a)
