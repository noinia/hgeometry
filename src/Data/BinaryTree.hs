{-# Language DeriveFunctor #-}
{-# Language FunctionalDependencies #-}
module Data.BinaryTree where


import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Tree as Tree

--------------------------------------------------------------------------------

data BinLeafTree v a = Leaf a
                     | Node (BinLeafTree v a) v (BinLeafTree v a)
                     deriving (Show,Read,Eq,Ord,Functor)


class Semigroup v => Measured v a | a -> v where
  measure :: a -> v

-- | smart constructor
node     :: Measured v a => BinLeafTree v a -> BinLeafTree v a -> BinLeafTree v a
node l r = Node l (measure l <> measure r) r


instance Measured v a => Measured v (BinLeafTree v a) where
  measure (Leaf x)     = measure x
  measure (Node _ v _) = v


instance Foldable (BinLeafTree v) where
  foldMap f (Leaf a)     = f a
  foldMap f (Node l _ r) = foldMap f l `mappend` foldMap f r

instance Foldable1 (BinLeafTree v)

instance Traversable (BinLeafTree v) where
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node l v r) = Node <$> traverse f l <*> pure v <*> traverse f r

instance Measured v a => Semigroup (BinLeafTree v a) where
  l <> r = node l r

-- | Given a function to combine internal nodes into b's and leafs into b's,
-- traverse the tree bottom up, and combine everything into one b.
foldUp                  :: (b -> v -> b -> b) -> (a -> b) -> BinLeafTree v a -> b
foldUp _ g (Leaf x)     = g x
foldUp f g (Node l x r) = f (foldUp f g l) x (foldUp f g r)

asBalancedBinLeafTree    :: NonEmpty a -> BinLeafTree Size (Elem a)
asBalancedBinLeafTree ys = asBLT (length ys') ys'
  where
    ys' = F.toList ys

    asBLT _ [x] = Leaf (Elem x)
    asBLT n xs  = let h       = n `div` 2
                      (ls,rs) = splitAt h xs
                  in node (asBLT h ls) (asBLT (n-h) rs)

newtype Size = Size Int deriving (Show,Read,Eq,Num,Integral,Enum,Real,Ord)

instance Semigroup Size where
  x <> y = x + y

newtype Elem a = Elem { _unElem :: a }
               deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Measured Size (Elem a) where
  measure _ = 1


--------------------------------------------------------------------------------
-- * Converting into a Data.Tree

data RoseElem v a = InternalNode v | LeafNode a deriving (Show,Eq,Functor)

toRoseTree              :: BinLeafTree v a -> Tree.Tree (RoseElem v a)
toRoseTree (Leaf x)     = Tree.Node (LeafNode x) []
toRoseTree (Node l v r) = Tree.Node (InternalNode v) (map toRoseTree [l,r])


drawTree :: (Show v, Show a) => BinLeafTree v a -> String
drawTree = Tree.drawTree . fmap show . toRoseTree
