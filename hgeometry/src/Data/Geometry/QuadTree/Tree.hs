{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.Geometry.QuadTree.Tree where


import           Control.Lens (makePrisms)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Apply
import           Data.Geometry.Point
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Quadrants
import           Data.Geometry.QuadTree.Split
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable.Class
import           Data.Semigroup.Traversable.Class
import qualified Data.Tree as RoseTree
import           Data.Tree.Util (TreeNode(..))

--------------------------------------------------------------------------------

-- | Our cells use Rational numbers as their numeric type
-- type CellR = Cell (RealNumber 10)

-- | The Actual Tree type representing a quadTree
data Tree v p = Leaf !p
              | Node !v (Quadrants (Tree v p)) -- quadrants are stored lazily on purpose
              deriving (Show,Eq)
makePrisms ''Tree

instance Bifunctor Tree where
  bimap = bimapDefault

instance Bifoldable Tree where
  bifoldMap = bifoldMapDefault

instance Bitraversable Tree where
  bitraverse f g = \case
    Leaf p    -> Leaf <$> g p
    Node v qs -> Node <$> f v <*> traverse (bitraverse f g) qs

instance Bifoldable1 Tree
instance Bitraversable1 Tree where
  bitraverse1 f g = \case
    Leaf p    -> Leaf <$> g p
    Node v qs -> Node <$> f v <.> traverse1 (bitraverse1 f g) qs

-- | Fold on the Tree type.
foldTree     :: (p -> b) -> (v -> Quadrants b -> b) -> Tree v p -> b
foldTree f g = go
  where
    go = \case
      Leaf p    -> f p
      Node v qs -> g v (go <$> qs)

-- | Produce a list of all leaves of a quad tree
leaves :: Tree v p -> NonEmpty p
leaves = NonEmpty.fromList . bifoldMap (const []) (:[])

-- | Converts into a RoseTree
toRoseTree :: Tree v p -> RoseTree.Tree (TreeNode v p)
toRoseTree = foldTree (\p    -> RoseTree.Node (LeafNode p)     [])
                      (\v qs -> RoseTree.Node (InternalNode v) (F.toList qs))

-- | Computes the height of the quadtree
height :: Tree v p -> Integer
height = foldTree (const 1) (\_ -> (1 +) . maximum)


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Functions operating on the QuadTree (in temrs of the 'Tree' type)

-- | Builds a QuadTree
build             :: Fractional r
                  => Splitter r pts v p -> Cell r -> pts -> Tree v p
build shouldSplit = build'
  where
    build' cc pts = case shouldSplit cc pts of
                      No p     -> Leaf p
                      Yes v qs -> Node v $ build' <$> splitCell cc <*> qs

-- | Annotate the tree with its corresponing cells
withCells :: Fractional r => Cell r -> Tree v p -> Tree (v :+ Cell r) (p :+ Cell r)
withCells c0 = \case
  Leaf p    -> Leaf (p :+ c0)
  Node v qs -> Node (v :+ c0) (withCells <$> splitCell c0 <*> qs)


--------------------------------------------------------------------------------


-- | Build a QuadtTree from a set of points.
--
-- pre: the points lie inside the initial given cell.
--
-- running time: \(O(nh)\), where \(n\) is the number of points and
-- \(h\) is the height of the resulting quadTree.
fromPoints :: (Fractional r, Ord r)
           => Cell r -> [Point 2 r :+ p]
           -> Tree () (Maybe (Point 2 r :+ p))
fromPoints = build fromPointsF

-- | The function that can be used to build a quadTree 'fromPoints'
fromPointsF   :: (Fractional r, Ord r)
              => Splitter r [Point 2 r :+ p] () (Maybe (Point 2 r :+ p))
fromPointsF c = \case
      []   -> No Nothing
      [p]  -> No (Just p)
      pts  -> Yes () $ partitionPoints c pts
        -- (\cell -> filter (`inCell` cell) pts) <$> splitCell c
