module Data.Tree.Util where

import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Control.Lens
import           Control.Monad ((>=>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (listToMaybe,maybeToList)
import           Data.Tree

--------------------------------------------------------------------------------

-- $setup
-- >>> :{
-- let myTree = Node 0 [ Node 1 []
--                     , Node 2 []
--                     , Node 3 [ Node 4 [] ]
--                     ]
-- :}


--------------------------------------------------------------------------------

-- | Nodes in a tree are typically either an internal node or a leaf node
data TreeNode v a = InternalNode v | LeafNode a deriving (Show,Eq)

instance Bifunctor TreeNode where
  bimap = bimapDefault
instance Bifoldable TreeNode where
  bifoldMap = bifoldMapDefault
instance Bitraversable TreeNode where
  bitraverse f g = \case
    InternalNode v -> InternalNode <$> f v
    LeafNode l     -> LeafNode     <$> g l

-- | A TreeNode is isomorphic to Either
_TreeNodeEither :: Iso' (TreeNode v p) (Either v p)
_TreeNodeEither = iso tne etn
  where
    tne = \case
      InternalNode v -> Left v
      LeafNode l     -> Right l
    etn = either InternalNode LeafNode

--------------------------------------------------------------------------------
-- * Zipper on rose trees

-- | Zipper for rose trees
data Zipper a = Zipper { focus      :: Tree a
                       , ancestors  :: [([Tree a], a, [Tree a])] -- left siblings in reverse order
                       }
              deriving (Show,Eq)

-- | Create a new zipper focussiong on the root.
root :: Tree a -> Zipper a
root = flip Zipper []

-- | Move the focus to the parent of this node.
up               :: Zipper a -> Maybe (Zipper a)
up (Zipper t as) = case as of
                     []              -> Nothing
                     ((ls,p,rs):as') -> Just $ Zipper (Node p (reverse ls <> [t] <> rs)) as'

-- | Move the focus to the first child of this node.
--
-- >>> firstChild $ root myTree
-- Just (Zipper {focus = Node {rootLabel = 1, subForest = []}, ancestors = [([],0,[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = []}]}])]})
firstChild                          :: Zipper a -> Maybe (Zipper a)
firstChild (Zipper (Node x chs) as) = case chs of
                                        []       -> Nothing
                                        (c:chs') -> Just $ Zipper c (([],x,chs'):as)

-- | Move the focus to the next sibling of this node
--
-- >>> (firstChild $ root myTree) >>= nextSibling
-- Just (Zipper {focus = Node {rootLabel = 2, subForest = []}, ancestors = [([Node {rootLabel = 1, subForest = []}],0,[Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = []}]}])]})
nextSibling               :: Zipper a -> Maybe (Zipper a)
nextSibling (Zipper t as) = case as of
                              []                  -> Nothing -- no parent
                              ((_,_,[]):_)        -> Nothing -- no next sibling
                              ((ls,p,(r:rs)):as') -> Just $ Zipper r ((t:ls,p,rs):as')

-- | Move the focus to the next sibling of this node
prevSibling               :: Zipper a -> Maybe (Zipper a)
prevSibling (Zipper t as) = case as of
                              []                  -> Nothing -- no parent
                              (([],_,_):_)        -> Nothing -- no prev sibling
                              (((l:ls),p,rs):as') -> Just $ Zipper l ((ls,p,t:rs):as')

-- | Given a zipper that focussses on some subtree t, construct a list with
-- zippers that focus on each child.
allChildren :: Zipper a -> [Zipper a]
allChildren = List.unfoldr ((\ch -> (ch, nextSibling ch)) <$>) . firstChild

-- | Given a zipper that focussses on some subtree t, construct a list with
-- zippers that focus on each of the nodes in the subtree of t.
allTrees   :: Zipper a -> [Zipper a]
allTrees r = r : concatMap allTrees (allChildren r)

-- | Creates a new tree from the zipper that thas the current node as root. The
-- ancestorTree (if there is any) forms the first child in this new root.
unZipperLocal                          :: Zipper a -> Tree a
unZipperLocal (Zipper (Node x chs) as) = Node x (maybeToList (constructTree as) <> chs)

-- | Constructs a tree from the list of ancestors (if there are any)
constructTree :: [([Tree a],a,[Tree a])] -> Maybe (Tree a)
constructTree = listToMaybe
              . foldr (\(ls,p,rs) tas -> [Node p (tas <> reverse ls <> rs)]) []


--------------------------------------------------------------------------------

-- | Given a predicate on an element, find a node that matches the predicate, and turn that
-- node into the root of the tree.
--
-- running time: \(O(nT)\) where \(n\) is the size of the tree, and \(T\) is
-- the time to evaluate a predicate.
--
-- >>> findEvert (== 4) myTree
-- Just (Node {rootLabel = 4, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = []},Node {rootLabel = 2, subForest = []}]}]}]})
-- >>> findEvert (== 5) myTree
-- Nothing
findEvert   :: (a -> Bool) -> Tree a -> Maybe (Tree a)
findEvert p = findEvert' (p . rootLabel)

-- | Given a predicate matching on a subtree, find a node that matches the predicate, and turn that
-- node into the root of the tree.
--
-- running time: \(O(nT(n))\) where \(n\) is the size of the tree, and \(T(m)\) is
-- the time to evaluate a predicate on a subtree of size \(m\).
findEvert'   :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
findEvert' p = fmap unZipperLocal . listToMaybe . filter (p . focus) . allTrees . root

-- | Function to extract a path between a start node and an end node (if such a
--path exists). If there are multiple paths, no guarantees are given about
--which one is returned.
--
-- running time: \(O(n(T_p+T_s)\), where \(n\) is the size of the tree, and
-- \(T_p\) and \(T_s\) are the times it takes to evaluate the 'isStartingNode'
-- and 'isEndingNode' predicates.
--
--
-- >>> findPath (== 1) (==4) myTree
-- Just [1,0,3,4]
-- >>>  findPath (== 1) (==2) myTree
-- Just [1,0,2]
-- >>>  findPath (== 1) (==1) myTree
-- Just [1]
-- >>>  findPath (== 1) (==2) myTree
-- Just [1,0,2]
-- >>>  findPath (== 4) (==2) myTree
-- Just [4,3,0,2]
findPath               :: (a -> Bool) -- ^ is this node a starting node
                          -> (a -> Bool) -- ^ is this node an ending node
                          -> Tree a -> Maybe [a]
findPath isStart isEnd = findEvert isStart >=> findNode isEnd

-- | Given a predicate on a, find (the path to) a node that satisfies the predicate.
--
-- >>> findNode (== 4) myTree
-- Just [0,3,4]
findNode   :: (a -> Bool) -> Tree a -> Maybe [a]
findNode p = listToMaybe . findNodes (p . rootLabel)

-- | Find all paths to nodes that satisfy the predicate
--
-- running time: \(O(nT(n))\) where \(n\) is the size of the tree, and \(T(m)\) is
-- the time to evaluate a predicate on a subtree of size \(m\).
--
-- >>> findNodes ((< 4) . rootLabel) myTree
-- [[0],[0,1],[0,2],[0,3]]
-- >>> findNodes (even . rootLabel) myTree
-- [[0],[0,2],[0,3,4]]
-- >>> let size = length in findNodes ((> 1) . size) myTree
-- [[0],[0,3]]
findNodes   :: (Tree a -> Bool) -> Tree a -> [[a]]
findNodes p = go
  where
    go t = let mh = if p t then [[]] else []
           in map (rootLabel t:) $ mh <> concatMap go (children t)


-- | BFS Traversal of the rose tree that decomposes it into levels.
--
-- running time: \(O(n)\)
levels :: Tree a -> NonEmpty (NonEmpty a)
levels = go1 . (:| [])
  where
    go0   :: [Tree a] -> [NonEmpty a]
    go0 q = case NonEmpty.nonEmpty q of
              Nothing -> []
              Just q1 -> NonEmpty.toList $ go1 q1
    {-# INLINE go0 #-}

    -- all work essentially happens here: given a bunch of trees whose
    -- root elements all have the same level, extract the values
    -- stored at these root nodes, collect all children in a big list,
    -- and explore those recursively.
    go1    :: NonEmpty (Tree a) -> NonEmpty (NonEmpty a)
    go1 qs = fmap root' qs :| go0 (concatMap children' qs)
    {-# INLINE go1 #-}

    root'     (Node x _)   = x
    children' (Node _ chs) = chs
