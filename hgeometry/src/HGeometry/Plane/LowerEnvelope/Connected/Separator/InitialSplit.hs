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

  , findNodeAlongPath
  ) where

import Control.Applicative
import Control.Lens ((<&>))
import Data.Bifunctor
import Data.Maybe (maybeToList)
import Data.Tree (Tree(..))
import HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
import HGeometry.Vector

-- import Debug.Trace
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


--------------------------------------------------------------------------------

-- | Search along a path; we search among the nodees on the path and in the subtrees
-- hanging off the path on the given side.
findNodeAlongPath        :: forall a. Show a =>
                            (a -> Bool)
                         -> Side -- ^ indicates which subtrees to search
                         -> Path a [Tree a] (NodeSplit a [Tree a])
                         -> Maybe ( InitialSplit a (Tree a)
                                  , Path a [Tree a] (Tree a)
                                  )
findNodeAlongPath p side = fmap (first reroot) . snd . foldPath leaf node
  where
    leaf :: NodeSplit a [Tree a] -> ( Path a [Tree a] (Tree a) -- old path
                                    , Maybe ( Path a [Tree a] (InitialSplit a (Tree a))
                                            , Path a [Tree a] (Tree a))) -- new path
    leaf ns@(NodeSplit u before after) = (Leaf t, res)
      where
        t = Node u $ before <> after
        res
          | p u = Just ( Leaf $ DecendantSplit u before
                                                 (error $ "findNodeAlongPath " <> show u)
                                                 after
                       , Leaf t
                       )
          | otherwise = here ns Nothing

    node ns@(NodeSplit u before after) (oldPath,res) =
        ( pathNode (u, oldPath) before after, res')
      where
        res'
          | p u       = Just ( Leaf $ DecendantSplit u before oldPath after
                             , Leaf $ Node u $ before <> [pathToTree oldPath] <> after
                             )
          | otherwise = extend <$> ((here ns (Just oldPath)) <|> res)
        extend = bimap extend' extend'
        extend'         :: Path a [Tree a] l -> Path a [Tree a] l
        extend' newPath = pathNode (u,newPath) before after

    pathToTree'' = fmap pathToTree . maybeToList

    -- Tries to search here. If found, returns a tuple in which the first element
    -- represnets the new split, and the second element is the new path. I.e. the path to
    -- the element we are searching for. The first element actually returns both the split
    -- as if it was an internal split and if it was a decentand split. This way we can use
    -- this function for both the node and the leaf case.
    here :: NodeSplit a [Tree a]
         -> Maybe (Path a [Tree a] (Tree a))
         -> Maybe ( Path a [Tree a] (InitialSplit a (Tree a)), Path a [Tree a] (Tree a))
    here (NodeSplit u before after) mOldPath = case side of
        L -> findNode' p before <&> \(NodeSplit path' before' after') ->
               (Leaf $ case mOldPath of
                     Nothing      -> DecendantSplit u before' path' (after' <> after)
                     Just oldPath ->
                       InternalSplit u (Split (Vector2 path' oldPath) before' after' after)
               , Path $ NodeSplit (u, path') before' (after' <> pathToTree'' mOldPath <> after)
              )
        -- Search on the left; i.e. in the before part
        R -> findNode' p after <&> \(NodeSplit path' before' after') ->
              (Leaf $ case mOldPath of
                 Nothing      -> DecendantSplit u (before <> before') path' after'
                 Just oldPath ->
                   InternalSplit u (Split (Vector2 oldPath path') before before' after')
              , Path $ NodeSplit (u, path') (before <> pathToTree'' mOldPath <> before') after'
              )

-- seems that the root eelm gets ignored
