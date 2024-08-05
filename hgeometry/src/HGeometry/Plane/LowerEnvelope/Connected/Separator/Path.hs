{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Path
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
  ( NodeSplit(..)
  , nodeSplitToTree
  , nodeSplitToTreeWith

  , Path(..)
  , trimap, trifoldMap
  , foldPath

  , pathToTree, pathToTree'
  , collectPath
  , pathWeight
  , endPoint

  , findNode
  , findNode'
  -- , findNodeAlongPath
  , Side(..)
  ) where

import           Control.Lens ((<&>))
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Foldable as F
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight


--------------------------------------------------------------------------------
-- * Paths

-- | A Node split represents the split of a node in a Rose-tree whose list of children
-- has been split into two parts; a before and and after part.
data NodeSplit a trees = NodeSplit a trees trees
  deriving (Show,Eq,Functor,Foldable)

instance Bifunctor NodeSplit where
  bimap f g (NodeSplit x as bs) = NodeSplit (f x) (g as) (g bs)
instance Bifoldable NodeSplit where
  bifoldMap f g (NodeSplit x as bs) = f x <> g as <> g bs

instance (Semigroup a, Semigroup trees) => Semigroup (NodeSplit a trees) where
  (NodeSplit x befores afters) <> (NodeSplit x' befores' afters') =
    NodeSplit (x <> x') (befores <> befores') (afters <> afters')
instance (Monoid a, Monoid trees) => Monoid (NodeSplit a trees) where
  mempty = NodeSplit mempty mempty mempty

-- | Computes the weight of a ndoesplit
nodeSplitWeight :: (Num w, IsWeight w) => Side -> NodeSplit a [Tree (Weighted w b)] -> w
nodeSplitWeight s (NodeSplit _ before after) = case s of
                                                 L -> weightOf before
                                                 R -> weightOf after

-- | unsplit the node split into a proper Tree
nodeSplitToTree :: NodeSplit a [Tree a] -> Tree a
nodeSplitToTree = flip nodeSplitToTreeWith []

-- | unsplit the node split into a proper Tree, adding the additional trees in the middle
nodeSplitToTreeWith                                   :: NodeSplit a [Tree a]
                                                      -> [Tree a] -> Tree a
nodeSplitToTreeWith (NodeSplit u before after) middle = Node u $ before <> middle <> after

--------------------------------------------------------------------------------

-- | A path in a rose tree; the last element of the path (a Leaf) stores an l, each node
-- in the path stores the value at that node, (and the remaining path) and the trees left
-- and right of the child-node the path visits.
data Path a trees l = Leaf l
                    | Path (NodeSplit (a,Path a trees l) trees)
                    deriving (Show,Eq)

instance Functor (Path a trees) where
  fmap = second
instance Bifunctor (Path a) where
  bimap = trimap id

-- | Fold on a path
foldPath :: (l -> r) -> (NodeSplit a trees -> r -> r) -> Path a trees l -> r
foldPath leaf node = go
  where
    go = \case
      Leaf l                                 -> leaf l
      Path (NodeSplit (x,path) before after) -> node (NodeSplit x before after) (go path)

-- | Trimap over a path
trimap :: (a -> a') -> (trees -> trees') -> (l -> l') -> Path a trees l -> Path a' trees' l'
trimap fa ft fl = go
  where
    go = \case
      Leaf x                          -> Leaf (fl x)
      Path (NodeSplit (x,path) xs ys) -> Path (NodeSplit (fa x,go path) (ft xs) (ft ys))

-- | fold over some path
trifoldMap          :: Monoid m => (a -> m) -> (trees -> m) -> (l -> m) -> Path a trees l -> m
trifoldMap fa ft fl = go
  where
    go = \case
      Leaf l                          -> fl l
      Path (NodeSplit (x,path) xs ys) -> fa x <> ft xs <> ft ys <> go path

----------------------------------------

-- | returns the a's left, on, and right of the path
collectPath :: Path a [Tree a] (NodeSplit a [Tree a]) -> NodeSplit [a] [a]
collectPath = foldPath handle (\ns r -> handle ns <> r)
  where
    handle (NodeSplit x before after) = NodeSplit [x] (f before) (f after)
    f = foldMap F.toList

-- | Get the endpoint of the path
endPoint :: Path a trees (NodeSplit a trees) -> a
endPoint = foldPath (\(NodeSplit x _ _) -> x) (\_ l -> l)

--------------------------------------------------------------------------------
-- * Searching a node on a path

-- | Tries to find the node in the given tree. Returns the path to this tree
-- if it can be found.
findNode      :: (a -> Bool) -> Tree a -> Maybe (Path a [Tree a] (Tree a))
findNode p t@(Node u chs)
  | p u       = Just $ Leaf t
  | otherwise = findNode' p chs <&> \(NodeSplit path before' after') ->
                                      Path $ NodeSplit (u,path) before' after'

-- | Find the node among the given trees. Returns essentially the path to this node, if it
-- can be found.
findNode'   :: (a -> Bool) -> [Tree a] -> Maybe (NodeSplit (Path a [Tree a] (Tree a)) [Tree a])
findNode' p = go
  where
    go = either (const Nothing) Just . foldr f (Left [])
    f t@(Node u chs) = \case
      Left after | p u                    -> Right $ NodeSplit (Leaf t) [] after
                 | otherwise              -> case go chs of
         Nothing                              -> Left (t:after)
         Just (NodeSplit path before' after') -> Right $ NodeSplit path' [] after
           where
             path' = Path $ NodeSplit (u,path) before' after'
      Right (NodeSplit path before after) -> Right $ NodeSplit path (t:before) after

-- | Recombine the path into a tree
pathToTree :: Path a [Tree a] (Tree a) -> Tree a
pathToTree = foldPath id (\ns ch -> nodeSplitToTreeWith ns [ch])

-- | Recombines a path ending in a nodesplit to a tree.
pathToTree' :: Path a [Tree a] (NodeSplit a [Tree a]) -> Tree a
pathToTree' = foldPath nodeSplitToTree (\ns ch -> nodeSplitToTreeWith ns [ch])
-- I coulud also have just used fmap nodeSplitToTree I guess. Hoping this may be slightly
-- more efficient.

-- | Computes the weight of the path on the particular side.
pathWeight   :: (IsWeight w, Num w)
             => Side -> Path c [Tree (Weighted w a)] (NodeSplit b [Tree (Weighted w d)]) -> w
pathWeight s = foldPath (nodeSplitWeight s) (\ns acc -> acc + nodeSplitWeight s ns)

--------------------------------------------------------------------------------

-- | Either left or Right
data Side = L | R deriving (Show,Eq)
