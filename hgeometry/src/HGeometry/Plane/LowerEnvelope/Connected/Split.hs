--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Split
  ( toSeparator

  ) where

import           Control.Applicative
import           Control.Lens ((<&>))
import qualified Data.Foldable as F
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Vector

--------------------------------------------------------------------------------

data NodeSplit a trees = NodeSplit a trees trees
  deriving (Show,Eq,Functor)

data Path a trees l = Leaf l
                    | Path (NodeSplit (a,Path a trees l) trees)
                    deriving (Show,Eq)

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


findNodeAlongPath   :: (a -> Bool)
                    -> Path a [Tree a] (Tree a)
                    -> Maybe (Path a [Tree a] (Tree a), Cycle a)
findNodeAlongPath p = undefined
-- \case
--     Leaf t@(Node u _) -> findNode p t <&> \path -> (path, leafSplit path)
--     Path (NodeSplit (u, path') before after)
--       | p u       -> Just $ ( Leaf (Node u $ before <> after)
--                             , Split (RootSplit undefined) before after undefined
--                             )
--       | otherwise -> undefined
--   where
--     leafSplit = \case
--         Leaf _                                  ->
--           error "findNodealongPath. Absurd. same node?"
--         Path (NodeSplit (w,path) before after) ->
--           Split (RootSplit rootSplit) before' middle after'
--           where
--             rootSplit = undefined
--             before' = undefined
--             middle = undefined
--             after' = undefined

findNodeAlongPath'   :: (a -> Bool)
                    -> Path a [Tree a] (Tree a)
                    -> Maybe (Path a [Tree a] (Tree a)
                             -- , InitialSplit a (Tree a)
                             )
findNodeAlongPath' p = go
  where
    go = \case
      Leaf t        -> findNode p t
      Path (NodeSplit (u, path) before after)
          | p u       -> Just $ Leaf (Node u $ before <> after)
          | otherwise ->     (mkBefore <$> findNode' p before)
                         <|> (mkAfter  <$> findNode' p after)
                         <|> (mkPath   <$> go path)
        where
          mkBefore (NodeSplit path' before' after') =
            Path $ NodeSplit (u, path') before' (after' <> after)
          mkAfter  (NodeSplit path' before' after') =
            Path $ NodeSplit (u, path') (before <> before') after'
          mkPath   path' = Path $ NodeSplit (u, path') before after


-- Two paths that split the subtree into three subtrees
data Split paths trees = Split paths trees trees trees
  deriving (Show,Eq,Functor,Foldable)

type RootPath a = a

-- | In cas of a root split, one path is not really a path, just the label of the root
-- and the other is a proper path. The main point is that the rootpath occurs before (Left)
-- or after (Right) of the actual path to the other node
data RootSplitPath a tree = RootBefore (RootPath a)         (Path a [tree] tree)
                          | RootAfter  (Path a [tree] tree) (RootPath a)
                          deriving (Show,Eq)

-- | Result of the initial split; we find a root split (say when w is a decentant of v)
-- or a proper node split.
data InitialSplit a tree =
    DecendantSplit a [tree] (Path a [tree] tree) [tree]
  | InternalSplit (Split (Vector 2 (Path a [tree] tree)) tree)
  deriving (Show,Eq)

data CycleSplitPaths a tree = RootSplit (RootSplitPath a tree)
                            | PathSplit (Path a [tree] tree) (Path a [tree] tree)
                            deriving (Show,Eq)

type Cycle a = Split (CycleSplitPaths a (Tree a)) [Tree a]


-- | the prefix should become part of the outside; in particular we put them on right of
-- the righstmost input leaf
splitTree :: Eq a => (a,a) -> Tree a -> Cycle a
splitTree = undefined

initialSplit         :: Eq a => (a,a) -> Tree a -> InitialSplit a (Tree a)
initialSplit (v,w) t =

  undefined

  go
  where
    path = findNode (== v) t

  --   go t@(Node u chs) =


makeInsideHeaviest                                         :: Cycle (Weighted' a)
                                                           -> Cycle (Weighted' a)
makeInsideHeaviest split@(Split paths before inside after)
  | weightOf inside < weightOf before + weightOf after =
      Split (shift paths) [] (after <> before) inside
  | otherwise = split
  where
    -- shift the paths
    shift = \case
      RootSplit (RootBefore r path) -> RootSplit (RootAfter path r)
      RootSplit (RootAfter path r)  -> RootSplit (RootBefore r path)
      PathSplit lPath rPath             -> PathSplit rPath lPath


type Weight = Int
type Weighted' = Weighted Weight

splitCycleAt                                     :: Eq a
                                                 => a
                                                 -> Cycle a
                                                 -> Vector 2 (Cycle a)
splitCycleAt u (Split paths before inside after) = case findNode' p inside of
    Just (NodeSplit path before' after') ->
      Vector2 (Split lPaths before              before' (after' <> after))
              (Split rPaths (before <> before') after' after)
      where
        (lPaths, rPaths) = case paths of
          RootSplit (RootBefore r rPath) -> ( RootSplit (RootBefore r path)
                                            , PathSplit path rPath
                                            )
          RootSplit (RootAfter lPath r)  -> ( PathSplit lPath path
                                            , RootSplit (RootAfter path r)
                                            )
          PathSplit lPath rPath          -> ( PathSplit lPath path
                                            , PathSplit path rPath
                                            )
    Nothing -> fromMaybe err $ splitCycleAtPath p paths before inside after
  where
    p = (== u) -- . getValue'
    err = error "splitCycleAt. absurd, node not found"

-- | Given a predicate that indicates the node we are trying to find, looks in the
-- subtrees hanging off of the paths/spines if we can find it, and returns the two cyclces
-- we get by splitting the trees there.
splitCycleAtPath :: (a -> Bool) -> CycleSplitPaths a (Tree a) -> [Tree a] -> [Tree a] -> [Tree a]
                 -> Maybe (Vector 2 (Cycle a))
splitCycleAtPath p paths before inside after = case paths of
   RootSplit (RootAfter lPath r)  ->
     (combineR (RootSplit . flip RootAfter r)) <$> findNodeAlongPath p lPath
   RootSplit (RootBefore r rPath) ->
     (combineL (RootSplit . RootBefore r))     <$> findNodeAlongPath p rPath
   PathSplit lPath rPath              ->
     (combineL (flip PathSplit rPath)          <$> findNodeAlongPath p lPath)
     <|>
     (combineR (PathSplit lPath)               <$> findNodeAlongPath p rPath)
  where
    -- creates a new split, where the new split is on the left, and the updated remainder
    -- of the current split is on the right.
    combineL f (path',split) = Vector2 split               (mkSplit $ f path')
    -- creates a new split, where the new split is on the right
    combineR f (path',split) = Vector2 (mkSplit $ f path') split
    mkSplit paths' = Split paths' before inside after


getValue' :: IsWeight w => Tree (Weighted w a) -> a
getValue' = getValue . root




toSeparator :: Weight -> Cycle (Weighted' a) -> ([a], Vector 2 [a])
toSeparator allowedWeight = toSeparator' allowedWeight . makeInsideHeaviest

toSeparator' :: Weight -> Cycle (Weighted' a) -> ([a], Vector 2 [a])
toSeparator' allowedWeight (Split paths before inside after)
  | weightOf inside <= allowedWeight =
      (toSep paths, Vector2 (toList' inside) (toList' before <> toList' after))
  | otherwise                       = undefined
    where
      toSep = undefined
      toList' = undefined

--------------------------------------------------------------------------------
class IsWeight w where
  data Weighted w :: Type -> Type
  getWeight :: Weighted w a -> w
  getValue  :: Weighted w a -> a

instance IsWeight Int where
  data Weighted Int a = Weighted {-#UNPACK#-}!Int a deriving (Show,Eq,Functor)
  getWeight (Weighted w _) = w


weightOf :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
weightOf = sum . map (getWeight . root)



-- | Annotate tht tree with the size of the subtrees
annotate              :: Tree k -> Tree (Weighted Int k)
annotate (Node v chs) = let chs' = map annotate chs
                        in Node (Weighted (1 + weightOf chs') v) chs'


root            :: Tree a -> a
root (Node v _) = v
