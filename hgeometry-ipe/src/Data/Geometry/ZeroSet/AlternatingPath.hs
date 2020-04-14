{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.ZeroSet.AlternatingPath where

import           Control.Lens ((^.))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

-- | An alternating path is a non-empty alternating sequence of
-- vertices and edges. There is one more vertex than there are edges.
data AlternatingPath e v = AlternatingPath v [e :+ v] deriving (Show,Eq)

instance Bifunctor AlternatingPath where
  bimap = bimapDefault
instance Bifoldable AlternatingPath where
  bifoldMap = bifoldMapDefault
instance Bitraversable AlternatingPath where
  bitraverse f g (AlternatingPath s es) = AlternatingPath <$> g s <*> traverse (bitraverse f g) es


-- | Computes the (start vertex, the edge sequence crossed, target vertex) if it exists
-- (and otherwise just returns the single vertex in the path)
alternatingFromTo :: AlternatingPath e v -> FromTo e v
alternatingFromTo = \case
  AlternatingPath s [] -> Singleton s
  AlternatingPath s xs -> FromTo s (NonEmpty.fromList $ map (^.core) xs) ((List.last xs)^.extra)


reversePath                          :: AlternatingPath e v -> AlternatingPath e v
reversePath p@(AlternatingPath s xs) = case xs of
    []             -> p
    ((e1 :+ _):tl) -> let ys = (e1 :+ s) : List.zipWith (\(_ :+ v) (e :+ _) -> e :+ v) xs tl
                          t  = (last xs)^.extra
                      in AlternatingPath t (reverse ys)




--------------------------------------------------------------------------------

-- | Models a path from some vertex v, via a bunch of edges e, to
-- another vertex v.  Note that if the source and the target vertex
-- are the same, the path just consists of a singleton vertex v.
data FromTo e v = Singleton !v
                | FromTo !v !(NonEmpty.NonEmpty e) !v
                deriving (Show,Eq,Read)

instance Bifunctor FromTo where
  bimap = bimapDefault
instance Bifoldable FromTo where
  bifoldMap = bifoldMapDefault
instance Bitraversable FromTo where
  bitraverse f g = \case
    Singleton x   -> Singleton <$> g x
    FromTo s es t -> FromTo <$> g s <*> traverse f es <*> g t
