module Data.Tree.Util where

import Data.Maybe(listToMaybe,maybeToList)
import Control.Lens
import Control.Monad((>=>))
import Data.Tree
import qualified Data.List as List

--------------------------------------------------------------------------------

-- $setup
-- >>> :{
-- let myTree = Node 0 [ Node 1 []
--                     , Node 2 []
--                     , Node 3 [ Node 4 [] ]
--                     ]
-- :}

--------------------------------------------------------------------------------
-- * Zipper on rose trees

-- | Zipper for rose trees
data Zipper a = Zipper { focus      :: Tree a
                       , ancestors  :: [([Tree a], a, [Tree a])] -- left siblings in reverse order
                       }
              deriving (Show,Eq)


root :: Tree a -> Zipper a
root = flip Zipper []

firstChild                          :: Zipper a -> Maybe (Zipper a)
firstChild (Zipper (Node x chs) as) = case chs of
                                        []       -> Nothing
                                        (c:chs') -> Just $ Zipper c (([],x,chs'):as)

up               :: Zipper a -> Maybe (Zipper a)
up (Zipper t as) = case as of
                     []              -> Nothing
                     ((ls,p,rs):as') -> Just $ Zipper (Node p (reverse ls <> [t] <> rs)) as'

nextSibling               :: Zipper a -> Maybe (Zipper a)
nextSibling (Zipper t as) = case as of
                              []                  -> Nothing -- no parent
                              ((_,_,[]):_)        -> Nothing -- no next sibling
                              ((ls,p,(r:rs)):as') -> Just $ Zipper r ((t:ls,p,rs):as')

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
--- [[0],[0,2],[0,3,4]]
-- >>> let size = length in findNodes ((> 1) . size) myTree
-- [[0],[0,3]]
findNodes   :: (Tree a -> Bool) -> Tree a -> [[a]]
findNodes p = go
  where
    go t = let mh = if p t then [[]] else []
           in map (rootLabel t:) $ mh <> concatMap go (children t)
