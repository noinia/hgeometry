module Data.BinaryTree.Zipper where

import Data.BinaryTree

--------------------------------------------------------------------------------

data Ctx a = Top | L (Ctx a) a (BinaryTree a) | R (BinaryTree a) a (Ctx a)
           deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

data BinaryTreeZipper a = Loc (BinaryTree a) (Ctx a)
           deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- | Focus on the root
top   :: BinaryTree a -> BinaryTreeZipper a
top t = Loc t Top

-- | Go to the left child
left                            :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
left (Loc (Internal l x r) ctx) = Just $ Loc l (L ctx x r)
left (Loc Nil _)                = Nothing

-- | Go to the right child
right                            :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
right (Loc (Internal l x r) ctx) = Just $ Loc r (R l x ctx)
right (Loc Nil _)                = Nothing

-- | Move to the parent
up                     :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
up (Loc _ Top)         = Nothing
up (Loc l (L ctx x r)) = Just $ Loc (Internal l x r) ctx
up (Loc r (R l x ctx)) = Just $ Loc (Internal l x r) ctx

-- | Navigate to the root
toRoot   :: BinaryTreeZipper a -> BinaryTreeZipper a
toRoot z = toRoot' z (Just z)
  where
    toRoot' z' Nothing   = z'
    toRoot' _  (Just z') = toRoot' z' (up z')


-- | Returns a list of zippers; one focussed on each node in the tree
visitAll   :: BinaryTree a -> [BinaryTreeZipper a]
visitAll t = visitAll' (top t)
  where
    f           = maybe [] visitAll'
    visitAll' z = z : f (left z) <> f (right z)

-- | Get the value stored at the current node
accessZ           :: BinaryTreeZipper a -> Maybe a
accessZ (Loc t _) = access t


-- | Returns all subtrees; i.e. every node with all its decendents
subTrees :: BinaryTree a -> [BinaryTree a]
subTrees t = Nil : subTrees' t
  where
    subTrees' Nil                 = []
    subTrees' tt@(Internal l _ r) = tt : subTrees' l <> subTrees' r


-- | Splits the tree here, returns a pair (innerTree,outerTree)
splitTree             :: BinaryTreeZipper a -> (BinaryTree a, BinaryTree a)
splitTree (Loc t ctx) = let (Loc r _) = toRoot $ Loc Nil ctx
                        in (t, r)
