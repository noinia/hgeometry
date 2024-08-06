--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Cycle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Represents a Cycle as two paths in the tree, and some information on how the leaves are
-- split.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Cycle
  ( Cycle
  , Cycle'
  , toCycle
  , splitTree
  , missingEdge

  , annotateCycle
  , makeInsideHeaviest

  , splitCycleAt

  , CycleSplitPaths(..)
  , collectPaths
  , cycleSplitPathWeights
  , endPoints
  , endPoints'

  , RootSplitPath(..)


  , Weight
  , Weighted'
  ) where

import           Control.Applicative
import           Control.Lens ((<&>))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Tree (Tree(..))
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.InitialSplit
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
import           HGeometry.Vector

import           Debug.Trace

--------------------------------------------------------------------------------

-- | The actual cycle
type Cycle a trees = Split (CycleSplitPaths a trees) trees
type Cycle' a = Cycle a [Tree a]

-- | The edge in the cycle that is not explicitly represented
missingEdge                     :: Cycle a [Tree a] -> (a, a)
missingEdge (Split paths _ _ _) = endPoints' paths

----------------------------------------

-- | The two paths that, together with the edge between their endpoints represent the
-- cycle. Both paths end int a NodeSplit. Generally, we have two non-empty paths (a
-- PathSplit). However, it may be that one of the two nodes is actually an ancestore of
-- the other one. This is represented by a RootSplit.
data CycleSplitPaths a trees = RootSplit (RootSplitPath a trees)
                             | PathSplit a -- ^ label of the splitting node
                                         (Path a trees (NodeSplit a trees))
                                         (Path a trees (NodeSplit a trees))
                            deriving (Show,Eq)

instance Functor (CycleSplitPaths a) where
  fmap = second
instance Bifunctor CycleSplitPaths where
  bimap f g = \case
    RootSplit rs            -> RootSplit $ bimap f g rs
    PathSplit r lPath rPath -> let h = trimap f g (bimap f g)
                               in PathSplit (f r) (h lPath) (h rPath)

instance Bifoldable CycleSplitPaths where
  bifoldMap f g = \case
    RootSplit rs            -> bifoldMap f g rs
    PathSplit r lPath rPath -> let h = trifoldMap f g (bifoldMap f g)
                               in f r <> h lPath <> h rPath


-- | Collects the paths into a (partial) separator
collectPaths                :: (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                            -> CycleSplitPaths a [Tree a] -> ([a], Vector 2 [a])
collectPaths splitChildren = \case
  RootSplit rs            -> collectRootSplitPath splitChildren rs
  PathSplit r lPath rPath -> let NodeSplit sepL before  middleL = collectPath lPath
                                 NodeSplit sepR middleR after   = collectPath rPath
                             in ( r : sepL <> sepR
                                , Vector2 (middleL <> middleR) (before <> after)
                                )
-- TODO: I don't think I need the splitChildren here!

-- | Computes the weights of the
cycleSplitPathWeights :: (Num w, IsWeight w) => CycleSplitPaths a [Tree (Weighted w b)]-> w
cycleSplitPathWeights = \case
  RootSplit rs            -> rootSplitWeight rs
  PathSplit _ lPath rPath -> pathWeight L lPath + pathWeight R rPath


-- | The labels of the leaves at which the cyclesplit paths end. If one is a root
-- splitpath the root comes first.
endPoints :: CycleSplitPaths a [Tree a] -> (a,a)
endPoints = \case
    RootSplit (RootBefore r path) -> (r, endPoint' path)
    RootSplit (RootAfter path r)  -> (r, endPoint' path)
    PathSplit _ lPath rPath       -> (endPoint' lPath, endPoint' rPath)

-- | Reports the endpoint of a path ending in a nodesplit
endPoint' :: Path a trees (NodeSplit c trees') -> c
endPoint' = splitRoot . endPoint

-- | The labels of the leaves at which the cyclesplit paths end. Left endpoint and then
-- right endpoint.
endPoints' :: CycleSplitPaths a [Tree a] -> (a,a)
endPoints' = \case
    RootSplit (RootBefore r path) -> (r, endPoint' path)
    RootSplit (RootAfter path r)  -> (endPoint' path, r)
    PathSplit _ lPath rPath       -> (endPoint' lPath, endPoint' rPath)


----------------------------------------

-- | In cas of a root split, one path is not really a path, just the label of the root
-- and the other is a proper path. The main point is that the rootpath occurs before (Left)
-- or after (Right) of the actual path to the other node
data RootSplitPath a trees = RootBefore (RootPath a)
                                        (Path a trees (NodeSplit a trees))
                           | RootAfter  (Path a trees (NodeSplit a trees))
                                        (RootPath a)
                          deriving (Show,Eq)
instance Functor (RootSplitPath a) where
  fmap = second
instance Bifunctor RootSplitPath where
  bimap f g = \case
    RootBefore r path -> RootBefore (f r) (trimap f g (bimap f g) path)
    RootAfter path r  -> RootAfter (trimap f g (bimap f g) path) (f r)

instance Bifoldable RootSplitPath where
  bifoldMap f g = \case
    RootBefore r path -> f r <> trifoldMap f g (bifoldMap f g) path
    RootAfter path r  -> trifoldMap f g (bifoldMap f g) path <> f r

-- | A root path itself is just the label of the root.
type RootPath a = a

-- | computes the weight of the paths hanging off a rootSplit
rootSplitWeight :: (IsWeight w, Num w) => RootSplitPath a [Tree (Weighted w b)] -> w
rootSplitWeight = \case
  RootBefore _ rPath -> pathWeight L rPath
  RootAfter lPath _  -> pathWeight R lPath

-- | Collect on a rootsplitPath
collectRootSplitPath                :: (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                    -> RootSplitPath a [Tree a] -> ([a], Vector 2 [a])
collectRootSplitPath _ = \case
    RootBefore r     rPath -> let NodeSplit sep inside outside = collectPath rPath
                              in (r:sep, Vector2 inside outside)
    RootAfter  lPath r     -> let NodeSplit sep outside inside = collectPath lPath
                              in (r:sep, Vector2 inside outside)


-- -- | Collect on a rootsplitPath
-- collectRootSplitPath                :: (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
--                                     -> RootSplitPath a [Tree a] -> ([a], Vector 2 [a])
-- collectRootSplitPath splitChildren = fromMaybe err . \case
--     RootBefore r path -> let (NodeSplit sep inside outside, before, after) = splitPath path in
--       splitChildren r before <&> \(Vector2 before' middle) ->
--           (r:sep, Vector2 (flatten middle <> inside) (flatten (before' <> after) <> outside))

--     RootAfter path r  -> let (NodeSplit sep outside inside, before, after) = splitPath path in
--       splitChildren r after <&> \(Vector2 middle after') ->
--           (r:sep, Vector2 (flatten middle <> inside) (flatten (before <> after') <> outside))
--   where
--     err = error "collectRootSplitPath: not found!?"
--     flatten = foldMap F.toList

--     splitPath = \case
--       Leaf (NodeSplit _ before after)         -> (mempty,           before,after)
--       Path (NodeSplit (_,path') before after) -> (collectPath path', before, after)


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Constructing a Cycle

type LeafSplitF     a = a       -- ^ the target to search for
                      -> Tree a -- ^ the leaf (i.e. a tree) to split
                      -> NodeSplit a [Tree a]
type ChildrenSplitF a = a        -- ^ the target to search for
                     -> a        -- ^ label of the node whose
                     -> [Tree a] -- ^ subset of the children we are splitting
                     -> Maybe (Vector 2 [Tree a])

-- | Transform an Initial split into a proper cycle by splitting the leaves (and the root
-- if needed).
--
-- given are:
-- - a function 'splitLeaf target theLeaf' that splits theLeaf based on target
-- - a function 'splitChildren target theNodeLabel theChildren' that splits theChildren
toCycle                          :: forall a.
                                    LeafSplitF a -> ChildrenSplitF a
                                 -> InitialSplit a (Tree a) -> Cycle a [Tree a]
toCycle splitLeaf splitChildren = \case
    InternalSplit v split              -> first (splitLeaves v) split
    DecendantSplit v before path after -> let t = root $ endPoint path
                                          in case splitChildren t v before of
      Nothing -> case splitChildren t v after of
        Nothing                          -> error "toCycle"
        Just (Vector2 middle after') ->
          Split (RootSplit $ RootAfter (splitLeaf v <$> path) v) before middle after'
      Just (Vector2 before' middle) ->
          Split (RootSplit $ RootBefore v (splitLeaf v <$> path)) before' middle after
  where
    splitLeaves :: a -> Vector 2 (Path a [Tree a] (Tree a)) -> CycleSplitPaths a [Tree a]
    splitLeaves v (Vector2 lPath rPath) = let l = root $ endPoint lPath
                                              r = root $ endPoint rPath
                                          in PathSplit v (splitLeaf r <$> lPath)
                                                         (splitLeaf l <$> rPath)

--------------------------------------------------------------------------------
-- | the prefix should become part of the outside; in particular we put them on right of
-- the righstmost input leaf
splitTree                           :: Eq a
                                    => LeafSplitF a -> ChildrenSplitF a
                                    -> (a,a)
                                    -> Tree a
                                    -> Cycle' a
splitTree splitLeaf splitChildren e = toCycle splitLeaf splitChildren . initialSplit e

--------------------------------------------------------------------------------

type Weight = Int
type Weighted' = Weighted Weight


-- | Annotates a cycle with the subtree weights.
annotateCycle :: Cycle' a -> Cycle' (Weighted Int a)
annotateCycle = bimap (bimap (Weighted 1) (fmap annotate)) (fmap annotate)

-- | Makes sure that the inside of the cycle is heaviest.
makeInsideHeaviest                                         :: Cycle' (Weighted' a)
                                                           -> Cycle' (Weighted' a)
makeInsideHeaviest split@(Split paths before inside after)
  | weightOf inside < weightOf before + weightOf after =
      Split (shift paths) [] (after <> before) inside
  | otherwise = split
  where
    -- shift the paths
    shift = \case
      RootSplit (RootBefore r path) -> RootSplit (RootAfter path r)
      RootSplit (RootAfter path r)  -> RootSplit (RootBefore r path)
      PathSplit r lPath rPath       -> PathSplit r rPath lPath

--------------------------------------------------------------------------------
{-
-- | Search along a path; we search among the nodees on the path and in the subtrees
-- hanging off the path on the given side.
--
-- Note that splitLeaf should already be applied so that it only takes the remaining leaf
findNodeAlongPath                   :: (     Tree a   -> NodeSplit a [Tree a])
                                    -> (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                    -> (a -> Bool)
                                    -> Side -- ^ indicates which subtrees to search
                                    -> Path a [Tree a] (NodeSplit a [Tree a])
                                    -> Maybe ( Cycle' a
                                             , Path a [Tree a] (Tree a)
                                             )
findNodeAlongPath splitLeaf splitChildren p side =


  go
  where
    go = \case
      Leaf (NodeSplit u before after)
        | p u       -> Just ( error "findNodeAlongPath; splitting the same leaf?"
                            , Leaf $ splitLeaf (Node u $ before <> after)
                            )
        | otherwise -> here u before after (RootSplit . flip RootAfter u)
                                           (RootSplit . RootBefore u)

      Path (NodeSplit (u, path) before after)
          | p u       -> Just ( cycle' u path before after
                              , Leaf $ splitLeaf (Node u $ before <> after)
                              )
          | otherwise -> here u before after (\path' -> PathSplit u path' path)
                                             (PathSplit u path)
                         <|> ( fmap (\path' -> Path $ NodeSplit (u,path') before after)
                              <$> go path)

    here u before after makeL makeR = case side of
        L -> findNode' p before <&> \(NodeSplit path' before' after') ->
                   let path'' = splitLeaf <$> path'
                   in ( Split (makeL path'') before' after' after
                      , Path $ NodeSplit (u, path'') before' (after' <> after)
                      )
             -- Search on the left; i.e. in the before part
        R -> findNode' p after <&> \(NodeSplit path' before' after') ->
                       let path'' = splitLeaf <$> path'
                       in ( Split (makeR path'') before before' after'
                          , Path $ NodeSplit (u, path'') (before <> before') after'
                          )

    cycle' u path before after = case splitChildren u before of
      Nothing -> case splitChildren u after of
        Nothing                          -> error "toCycle"
        Just (Vector2 middle after') ->
          Split (RootSplit $ RootAfter path u) before middle after'
      Just (Vector2 before' middle) ->
          Split (RootSplit $ RootBefore u path) before' middle after
-}

--------------------------------------------------------------------------------
-- * Splitting a Cycle


-- | Tries to split the cycle at the given node
splitCycleAt                                     :: forall a.
                                                    Show a =>
                                                    LeafSplitF a -> ChildrenSplitF a
                                                 -> (a -> Bool)
                                                 -> Cycle' a
                                                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAt splitLeaf splitChildren p theSplit@(Split paths before inside after)
       | traceShow ("splitCycleAt",paths,before,inside,after) False = undefined
       | otherwise = splitInterior <|> splitCycleAtPath splitLeaf splitChildren p theSplit
    where
    (l,r) = endPoints paths -- the endpoints of the two paths

    splitInterior = traceShowWith ("splitInterior",) $ constructCycles <$> findNode' p inside

    constructCycles :: NodeSplit (Path a [Tree a] (Tree a)) [Tree a]
                    -> Vector 2 (Cycle a [Tree a])
    constructCycles (NodeSplit path before' after') = Vector2 leftCycle rightCycle
      where
        -- unsplit the leaf, and then resplit it with the new target, which is in the middle.
        resplitLeaf :: NodeSplit a [Tree a] -> NodeSplit a [Tree a]
        resplitLeaf = splitLeaf (root $ endPoint path) . nodeSplitToTree

        -- in the left cycle we replace the right path of paths by the new path.
        -- note that we actually have to resplit the left path, and we have to
        -- appropriately split the new path with the left endpoint.
        -- note that the old right path turns into an additional tree after the split/cycle.
        leftCycle = Split lPaths before before' (after' <> lAfter <> after)
          where
            path' = splitLeaf l <$> path --
            (lPaths, lAfter) = case paths of
                RootSplit (RootBefore u rPath) -> ( RootSplit (RootBefore u path')
                                                  , [pathToTree' rPath]
                                                  )
                RootSplit (RootAfter lPath u)  -> ( PathSplit u (resplitLeaf <$> lPath) path'
                                                  , []
                                                  )
                PathSplit u lPath rPath        -> ( PathSplit u (resplitLeaf <$> lPath) path'
                                                  , [pathToTree' rPath]
                                                  )

        -- symmetric to before, now the new path is the new left path though, and the old
        -- left path becomes a new tree before the split.
        rightCycle = Split rPaths (before <> rBefore <> before') after' after
          where
            path' = splitLeaf r <$> path --
            (rPaths, rBefore) = case paths of
              RootSplit (RootBefore u rPath) -> ( PathSplit u  path' (resplitLeaf <$> rPath)
                                                , []
                                                )
              RootSplit (RootAfter lPath u)  -> ( RootSplit (RootAfter path' u)
                                                , [pathToTree' lPath]
                                                )
              PathSplit u lPath rPath        -> ( PathSplit u path' (resplitLeaf <$> rPath)
                                                , [pathToTree' lPath]
                                                )

-- | Given a predicate that indicates the node we are trying to find, looks in the
-- subtrees hanging off of the paths/spines if we can find it, and returns the two cyclces
-- we get by splitting the trees there.
splitCycleAtPath :: forall a. Show a =>
                    LeafSplitF a -> ChildrenSplitF a
                 -> (a -> Bool)
                 -> Cycle' a
                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAtPath splitLeaf splitChildren p (Split paths before middle after) =
    fmap toCycle' <$> (splitLeftPath <|> splitRightPath)
  where
    toCycle' = toCycle splitLeaf splitChildren

    splitLeftPath = case paths of
      RootSplit (RootBefore _ _)     -> Nothing -- there is no left path to split
      RootSplit (RootAfter lPath u)  -> findNodeAlongPath p R lPath <&> \(lSplit,lPath') ->
                Vector2 lSplit
                        (DecendantSplit u before lPath' (middle <> after))
      PathSplit u lPath rPath        -> findNodeAlongPath p R lPath <&> \(lSplit,lPath') ->
          let rPath' = nodeSplitToTree <$> rPath
          in Vector2 lSplit
                     (InternalSplit u (Split (Vector2 lPath' rPath') before middle after))

    splitRightPath = case paths of
      RootSplit (RootBefore u rPath) -> findNodeAlongPath p L rPath <&> \(rSplit,rPath') ->
                Vector2 (DecendantSplit u (before <> middle) rPath' after)
                        rSplit
      RootSplit (RootAfter _ _)      -> Nothing -- there is no right path to split
      PathSplit u lPath rPath        -> findNodeAlongPath p L rPath <&> \(rSplit,rPath') ->
          let lPath' = nodeSplitToTree <$> lPath
          in Vector2 (InternalSplit u (Split (Vector2 lPath' rPath') before middle after))
                     rSplit
