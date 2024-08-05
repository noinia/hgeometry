--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.InitialSplit
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Represents a split of a tree with two paths
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.InitialSplit
  ( Split(..)

  , InitialSplit(..)
  , initialSplit
  , initialSplitToTree
  ) where

import           Control.Lens ((<&>))
import           Data.Bifunctor
import           Data.Tree (Tree(..))
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * A Split

-- | Two paths that split the subtree into three subtrees.
data Split paths trees = Split paths trees trees trees
  deriving (Show,Eq,Functor,Foldable)

instance Bifunctor Split where
  bimap f g (Split paths as bs cs) = Split (f paths) (g as) (g bs) (g cs )

--------------------------------------------------------------------------------

-- | Result of the initial split; we find a root split (say when w is a decentant of v)
-- or a proper node split.
data InitialSplit a tree =
    DecendantSplit a [tree] (Path a [tree] tree) [tree]
  | InternalSplit a (Split (Vector 2 (Path a [tree] tree)) [tree])
  deriving (Show,Eq)

-- | Convert the initial split back into a tree.
initialSplitToTree :: InitialSplit a (Tree a) -> Tree a
initialSplitToTree = \case
  DecendantSplit u before path after -> Node u (before <> [pathToTree path] <> after)
  InternalSplit u (Split (Vector2 lPath rPath) before middle after) ->
    Node u (before <> [pathToTree lPath] <> middle <> [pathToTree rPath] <> after)

-- | Given two elements, computes a split of the tree by the paths to these two
-- elements. Moreover, we reroot the tree so that the node where the paths join becomes
-- the node represented by the split.
initialSplit         :: forall a. Eq a
                     => (a,a) -> Tree a -> InitialSplit a (Tree a)
initialSplit (v,w) t = maybe (error "initialSplit") reroot $
                         findNode (== v) t >>= go
  where
    go = \case
      Leaf (Node u chs)                       ->
        findW chs <&> \(NodeSplit path before after) ->
                        Leaf $ DecendantSplit u before path after
        -- in this case we have u == v
      Path (NodeSplit (u, pathV) before after)
        | u == w    -> Just . Leaf $ DecendantSplit u before pathV after
        | otherwise -> case findW before of
            Just (NodeSplit pathW before' middle) -> let paths = Vector2 pathW pathV
            -- FIXME: we should empty the first pathW's I think
                                                     in internalSplit u paths before' middle after
            Nothing -> case findW after of
              Just (NodeSplit pathW middle after') -> let paths = Vector2 pathV pathW
                                                      in internalSplit u paths before middle after'
              Nothing -> go pathV <&> \path' -> Path (NodeSplit (u, path') before after)

    findW = findNode' (== w)

    internalSplit u paths before middle after =
      Just . Leaf . InternalSplit u $ Split paths before middle after

-- TODO, Verify: I guess the first nodes of thw two paths should have empty before's and afters.

-- | Given a path to some split node; reroot the split so that the node the path leads to
-- essentially becomes the root of the tree.
reroot :: Path a [Tree a] (InitialSplit a (Tree a)) -> InitialSplit a (Tree a)
reroot = go []
  where
    addBefore up = \case
      DecendantSplit x before path after -> DecendantSplit x (up <> before) path after
      InternalSplit x (Split paths before middle after) ->
        InternalSplit x (Split paths (up <> before) middle after)

    go up = \case
      Leaf ns                                -> addBefore up ns
      Path (NodeSplit (u,path) before after) -> go [Node u (after <> up <> before)] path


-- reroot :: Path a [Tree a] (NodeSplit a [Tree a]) -> NodeSplit a [Tree a]
-- reroot = go []
--   where
--     addBefore up = \case
--       NodeSplit x before after -> NodeSplit x (up <> before) after

--     go up = \case
--       Leaf ns                                -> addBefore up ns
--       Path (NodeSplit (u,path) before after) -> go [Node u (after <> up <> before)] path
