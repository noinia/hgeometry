--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Tree.Binary.Static
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Several types of Binary trees.
--
--------------------------------------------------------------------------------
module HGeometry.Tree.Binary.Static
  ( BinLeafTree(..)
  , node
  , asBalancedBinLeafTree
  , foldUp, foldUpData

  , zipExactWith
  , toRoseTree
  , drawTree

  , BinaryTree(..)
  , asBalancedBinTree
  , access
  , foldBinaryUp
  , toRoseTree'
  , fromRoseTree'
  , drawTree'
  ) where

import           Control.DeepSeq
import           Data.Bifunctor.Apply
import qualified Data.Functor.Apply as Apply
import           Data.Maybe (mapMaybe)
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import           GHC.Generics (Generic)
import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.Foldable.Util
import           HGeometry.Measured.Class
import           HGeometry.Measured.Size
import           HGeometry.Tree.Util (TreeNode(..))

--------------------------------------------------------------------------------

-- | Binary tree that stores its values (of type a) in the leaves. Internal
-- nodes store something of type v.
data BinLeafTree v a = Leaf !a
                     | Node (BinLeafTree v a) !v (BinLeafTree v a)
                     deriving (Show,Read,Eq,Ord,Generic,Functor,Foldable,Traversable)

instance (NFData v, NFData a) => NFData (BinLeafTree v a)

-- | smart constructor
node     :: ( Measured f a, Semigroup (f a)
            ) => BinLeafTree (f a) a -> BinLeafTree (f a) a -> BinLeafTree (f a) a
node l r = Node l (getMeasure l <> getMeasure r) r


instance Bifunctor BinLeafTree where
  bimap f g = \case
    Leaf x     -> Leaf $ g x
    Node l k r -> Node (bimap f g l) (f k) (bimap f g r)

-- | Get the measure of a subtree
getMeasure :: Measured f a => BinLeafTree (f a) a -> f a
getMeasure = \case
  Leaf x     -> measure x
  Node _ v _ -> v

instance Foldable1 (BinLeafTree v) where
  foldMap1 f = \case
    Leaf x     -> f x
    Node l _ r -> foldMap1 f l <> foldMap1 f r

instance Traversable1 (BinLeafTree v) where
  traverse1 f = go
    where
      go = \case
        Leaf x     -> Leaf <$> f x
        Node l v r -> (\l' r' -> Node l' v r') <$> go l Apply.<.> go r

instance (Measured f a, Semigroup (f a)) => Semigroup (BinLeafTree (f a) a) where
  l <> r = node l r

-- | Create a balanced tree, i.e. a tree of height \(O(\log n)\) with the
-- elements in the leaves.
--
-- \(O(n)\) time.
asBalancedBinLeafTree :: Foldable1 f => f a -> BinLeafTree (Count a) a
asBalancedBinLeafTree = divideAndConquer1 Leaf
-- -- the implementation below produces slightly less high trees, but runs in
-- -- \(O(n \log n)\) time, as on every level it traverses the list passed down.
-- asBalancedBinLeafTree ys = asBLT (length ys') ys' where ys' = toList ys

--     asBLT _ [x] = Leaf (Elem x)
--     asBLT n xs  = let h       = n `div` 2
--                       (ls,rs) = splitAt h xs
--                   in node (asBLT h ls) (asBLT (n-h) rs)

-- | Given a function to combine internal nodes into b's and leafs into b's,
-- traverse the tree bottom up, and combine everything into one b.
foldUp                  :: (b -> v -> b -> b) -> (a -> b) -> BinLeafTree v a -> b
foldUp _ g (Leaf x)     = g x
foldUp f g (Node l x r) = f (foldUp f g l) x (foldUp f g r)


-- | Traverses the tree bottom up, recomputing the assocated values.
foldUpData     :: (w -> v -> w -> w) -> (a -> w) -> BinLeafTree v a -> BinLeafTree w a
foldUpData f g = foldUp f' Leaf
  where
    f' l v r = Node l (f (access' l) v (access' r)) r

    access' (Leaf x)     = g x
    access' (Node _ v _) = v

-- | Takes two trees, that have the same structure, and uses the provided
-- functions to "zip" them together
zipExactWith                                  :: (u -> v -> w)
                                              -> (a -> b -> c)
                                              -> BinLeafTree u a
                                              -> BinLeafTree v b
                                              -> BinLeafTree w c
zipExactWith _ g (Leaf x)     (Leaf y)        = Leaf (x `g` y)
zipExactWith f g (Node l m r) (Node l' m' r') = Node (zipExactWith f g l l')
                                                     (m `f` m')
                                                     (zipExactWith f g r r')
zipExactWith _ _ _            _               =
    error "zipExactWith: tree structures not the same "



--------------------------------------------------------------------------------
-- * Converting into a Data.Tree

-- | \( O(n) \) Convert binary tree to a rose tree, aka 'Tree.Tree'.
toRoseTree              :: BinLeafTree v a -> Tree.Tree (TreeNode v a)
toRoseTree (Leaf x)     = Tree.Node (LeafNode x) []
toRoseTree (Node l v r) = Tree.Node (InternalNode v) (map toRoseTree [l,r])

-- | 2-dimensional ASCII drawing of a tree.
drawTree :: (Show v, Show a) => BinLeafTree v a -> String
drawTree = Tree.drawTree . fmap show . toRoseTree

--------------------------------------------------------------------------------
-- * Internal Node Tree

-- | Binary tree in which we store the values of type a in internal nodes.
data BinaryTree a = Nil
                  | Internal (BinaryTree a) !a (BinaryTree a)
                  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Generic)
instance NFData a => NFData (BinaryTree a)


-- | Get the element stored at the root, if it exists
access                  :: BinaryTree a -> Maybe a
access Nil              = Nothing
access (Internal _ x _) = Just x

-- | Create a balanced binary tree.
--
-- running time: \(O(n)\)
asBalancedBinTree :: forall f a. Foldable f => f a -> BinaryTree a
asBalancedBinTree = mkTree . fromFoldable @V.Vector
  where
    mkTree v = let n = V.length v
                   h = n `div` 2
                   x = v V.! h
               in if n == 0 then Nil
                            else Internal (mkTree $ V.slice 0 h v) x
                                          (mkTree $ V.slice (h+1) (n - h -1) v)

-- | Fold function for folding over a binary tree.
foldBinaryUp                      :: b -> (a -> b -> b -> b)
                                  -> BinaryTree a -> BinaryTree (a,b)
foldBinaryUp _ _ Nil              = Nil
foldBinaryUp e f (Internal l x r) = let l' = foldBinaryUp e f l
                                        r' = foldBinaryUp e f r
                                        g  = maybe e snd . access
                                        b  = f x (g l') (g r')
                                    in Internal l' (x,b) r'

-- | Convert a @BinaryTree@ into a RoseTree
toRoseTree'                  :: BinaryTree a -> Maybe (Tree.Tree a)
toRoseTree' Nil              = Nothing
toRoseTree' (Internal l v r) = Just $ Tree.Node v $ mapMaybe toRoseTree' [l,r]

-- | Draw a binary tree.
drawTree' :: Show a => BinaryTree a -> String
drawTree' = maybe "Nil" (Tree.drawTree . fmap show) . toRoseTree'


-- | Tries to convert a rose Tree into a binary tree.
fromRoseTree'                   :: Tree.Tree a -> Maybe (BinaryTree a)
fromRoseTree' (Tree.Node x chs) = case traverse fromRoseTree' chs of
                                    Just []    -> Just $ Internal Nil x Nil
                                    Just [l]   -> Just $ Internal l   x Nil
                                    Just [l,r] -> Just $ Internal l   x r
                                    _          -> Nothing
