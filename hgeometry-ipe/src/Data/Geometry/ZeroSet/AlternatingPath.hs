{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.ZeroSet.AlternatingPath where

import           Algorithms.BinarySearch
import           Control.Lens (makeLenses, (^.), (%~), (.~), (&), (^?!), ix)
import           Data.Util
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine (PolyLine)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tree as RoseTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Split
import           Data.Geometry.Directions
import           Data.Geometry.QuadTree
import qualified Data.Geometry.QuadTree.Tree as Tree
import           Data.Maybe (maybeToList)



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
alternatingFromTo :: AlternatingPath e v -> Either v (v,NonEmpty e,v)
alternatingFromTo = \case
  AlternatingPath s [] -> Left s
  AlternatingPath s xs -> Right (s,NonEmpty.fromList $ map (^.core) xs, (List.last xs)^.extra)

--------------------------------------------------------------------------------
