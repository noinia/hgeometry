module Data.BinaryTree.Zipper where

import Data.BinaryTree
import Data.Semigroup

--------------------------------------------------------------------------------

data Ctx a = Top | L (Ctx a) a (BinaryTree a) | R (BinaryTree a) a (Ctx a)
           deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

data BinaryTreeZipper a = Loc (BinaryTree a) (Ctx a)
           deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- goLeft :: BinaryTreeZipper a -

top   :: BinaryTree a -> BinaryTreeZipper a
top t = Loc t Top

left                            :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
left (Loc (Internal l x r) ctx) = Just $ Loc l (L ctx x r)
left (Loc Nil _)                = Nothing

right                            :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
right (Loc (Internal l x r) ctx) = Just $ Loc r (R l x ctx)
right (Loc Nil _)                = Nothing

up                     :: BinaryTreeZipper a -> Maybe (BinaryTreeZipper a)
up (Loc _ Top)         = Nothing
up (Loc l (L ctx x r)) = Just $ Loc (Internal l x r) ctx
up (Loc r (R l x ctx)) = Just $ Loc (Internal l x r) ctx


toRoot   :: BinaryTreeZipper a -> BinaryTreeZipper a
toRoot z = toRoot' z (Just z)
  where
    toRoot' z' Nothing   = z'
    toRoot' _  (Just z') = toRoot' z' (up z')


visitAll   :: BinaryTree a -> [BinaryTreeZipper a]
visitAll t = visitAll' (top t)
  where
    f           = maybe [] visitAll'
    visitAll' z = z : f (left z) <> f (right z)

accessZ (Loc t _) = access t



-- | Returns all subtrees; i.e. every node with all its decendents
subTrees :: BinaryTree a -> [BinaryTree a]
subTrees t = Nil : subTrees' t
  where
    subTrees' Nil                 = []
    subTrees' tt@(Internal l _ r) = tt : subTrees' l <> subTrees' r


splitTree             :: BinaryTreeZipper a -> (BinaryTree a, BinaryTree a)
splitTree (Loc t ctx) = let (Loc r _) = toRoot $ Loc Nil ctx
                        in (t, r)
