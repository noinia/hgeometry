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
  ( Cycle(MkCycle,Cycle)
  , Cycle'
  , toCycle
  , splitTree
  , missingEdge
  , collectWith
  , toSeparator, separatorWeight

  , annotateCycle
  , makeInsideHeaviest

  , splitCycleAt

  , CycleSplitPaths(..)
  , collectPathsWith
  , collectAll
  -- , cycleSplitPathWeights
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
import qualified Data.Foldable as F
import qualified Data.Set as Set
import           Data.Tree (Tree(..))
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.InitialSplit
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Type
import           HGeometry.Vector

import           Debug.Trace

--------------------------------------------------------------------------------

type Cycle' a = Cycle a [Tree a]

-- | The actual cycle
newtype Cycle a trees = MkCycle (Split (CycleSplitPaths a trees) trees)
  deriving (Show,Eq)

pattern Cycle :: CycleSplitPaths a trees -> trees -> trees -> trees -> Cycle a trees
pattern Cycle paths before middle after = MkCycle (Split paths before middle after)
{-# COMPLETE Cycle #-}

instance Functor (Cycle a) where
  fmap = second
instance Bifunctor Cycle where
  bimap f g (MkCycle split) = MkCycle $ bimap (bimap f g) g split


-- | The edge in the cycle that is not explicitly represented
missingEdge                     :: Cycle a [Tree a] -> (a, a)
missingEdge (Cycle paths _ _ _) = endPoints' paths

-- | Collects all 'a's
collectAll  :: Show a =>
  Cycle' a -> [a]
collectAll = F.fold . toSeparator

-- | Collect stuff about  the separtor
collectWith                                     :: (Show a, Show w,
                                                     Monoid w
                                                   )
                                                => (a -> w)
                                                -> Cycle' a -> Separator w
collectWith f (Cycle paths before middle after) = here <> collectPathsWith f paths
  where
    f'   = foldMap (foldMap f)
    here = Separator mempty (f' middle) (f' $ before <> after)


----------------------------------------


-- | Turn the cycle into an actual separator.
toSeparator :: Show a => Cycle' a -> Separator [a]
toSeparator = fmap (getValue @Weight) . collectWith weigh
  -- collectWith (:[])

-- | Computes the weight of the separator, interior, and exterior
separatorWeight :: Show a =>
  Cycle' a -> Separator Weight
separatorWeight = fmap getWeight . collectWith weigh


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



-- | Collects all a's
collectAllPaths :: Show a =>
  CycleSplitPaths a [Tree a] -> [a]
collectAllPaths = F.fold . collectPathsWith (:[])

-- | Collects the paths into a (partial) separator
collectPathsWith   :: (Show a, Show w,
                       Monoid w
                      )
                   => (a -> w) -> CycleSplitPaths a [Tree a] -> Separator w
collectPathsWith f = \case
  RootSplit rs            -> collectRootSplitPathWith f rs
  PathSplit r lPath rPath -> let ll@(NodeSplit sepL before  middleL) = collectPathWith f lPath
                                 rr@(NodeSplit sepR middleR after)   = collectPathWith f rPath
                             in Separator (f r <> sepL <> sepR)
                                          (middleL <> middleR) (before <> after)

-- | The labels of the leaves at which the cyclesplit paths end. If one is a root
-- splitpath the root comes first.
endPoints :: CycleSplitPaths a [Tree a] -> (a,a)
endPoints = \case
    RootSplit (RootBefore r path) -> (r, endPoint' path)
    RootSplit (RootAfter path r)  -> (r, endPoint' path)
    PathSplit _ lPath rPath       -> (endPoint' lPath, endPoint' rPath)

-- | Reports the endpoint of a path ending in a nodesplit
endPoint' :: Path a trees (NodeSplit c trees') -> c
endPoint' = splitRootLabel . endPoint

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
rootSplitWeight :: RootSplitPath a [Tree a] -> Weight
rootSplitWeight = \case
  RootBefore _ rPath -> pathWeightOn L rPath
  RootAfter lPath _  -> pathWeightOn R lPath

-- | Collect on a rootsplitPath
collectRootSplitPathWith   :: Monoid w => (a -> w) -> RootSplitPath a [Tree a] -> Separator w
collectRootSplitPathWith f = \case
    RootBefore r     rPath -> let NodeSplit sep inside outside = collectPathWith f rPath
                              in Separator (f r <> sep) inside outside
    RootAfter  lPath r     -> let NodeSplit sep outside inside = collectPathWith f lPath
                              in Separator (f r <> sep) inside outside

flatten :: [Tree a] -> [a]
flatten = foldMap F.toList

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
toCycle splitLeaf splitChildren = MkCycle . \case
    InternalSplit v split              -> first (splitLeaves v) split
    DecendantSplit v before path after -> let t = rootLabel $ endPoint path
                                          in case splitChildren t v before of
      Nothing -> case splitChildren t v after of
        Nothing                          -> error "toCycle"
        Just (Vector2 middle after') ->
          Split (RootSplit $ RootAfter (splitLeaf v <$> path) v) before middle after'
      Just (Vector2 before' middle) ->
          Split (RootSplit $ RootBefore v (splitLeaf v <$> path)) before' middle after
  where
    splitLeaves :: a -> Vector 2 (Path a [Tree a] (Tree a)) -> CycleSplitPaths a [Tree a]
    splitLeaves v (Vector2 lPath rPath) = let l = rootLabel $ endPoint lPath
                                              r = rootLabel $ endPoint rPath
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
annotateCycle :: Cycle a [Tree a] -> Cycle (Weighted Weight a) [Tree (Weighted Weight a)]
annotateCycle = bimap (Weighted 1) (fmap annotate)

-- | Makes sure that the inside of the cycle is heaviest.
makeInsideHeaviest                                     :: Show a =>
  Cycle' a -> Cycle' a
makeInsideHeaviest c@(Cycle paths before inside after)
    | interiorWeight < exteriorWeight = Cycle (shift paths) [] (after <> before) inside
    | otherwise                       = c
  where
    Separator _ interiorWeight exteriorWeight = separatorWeight c
    -- shift the paths
    shift = \case
      RootSplit (RootBefore r path) -> RootSplit (RootAfter path r)
      RootSplit (RootAfter path r)  -> RootSplit (RootBefore r path)
      PathSplit r lPath rPath       -> PathSplit r rPath lPath

--------------------------------------------------------------------------------
-- * Splitting a Cycle

-- | Tries to split the cycle at the given node
splitCycleAt                                     :: forall a.
                                                    (Show a, Ord a) =>
                                                    LeafSplitF a -> ChildrenSplitF a
                                                 -> (a -> Bool)
                                                 -> Cycle' a
                                                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAt splitLeaf splitChildren p theSplit@(Cycle paths before inside after) =
      splitInterior <|> splitCycleAtPath splitLeaf splitChildren p theSplit
    where
    (l,r) = endPoints paths -- the endpoints of the two paths

    splitInterior = constructCycles <$> findNode' p inside

    constructCycles :: NodeSplit (Path a [Tree a] (Tree a)) [Tree a]
                    -> Vector 2 (Cycle a [Tree a])
    constructCycles (NodeSplit path before' after') = Vector2 leftCycle rightCycle
      where
        -- unsplit the leaf, and then resplit it with the new target, which is in the middle.
        resplitLeaf :: NodeSplit a [Tree a] -> NodeSplit a [Tree a]
        resplitLeaf = splitLeaf (rootLabel $ endPoint path) . nodeSplitToTree

        -- in the left cycle we replace the right path of paths by the new path.
        -- note that we actually have to resplit the left path, and we have to
        -- appropriately split the new path with the left endpoint.
        -- note that the old right path turns into an additional tree after the split/cycle.
        leftCycle = Cycle lPaths before before' (after' <> lAfter <> after)
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
        rightCycle = Cycle rPaths (before <> rBefore <> before') after' after
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
splitCycleAtPath :: forall a. (Show a, Ord a) =>
                    LeafSplitF a -> ChildrenSplitF a
                 -> (a -> Bool)
                 -> Cycle' a
                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAtPath splitLeaf splitChildren p (Cycle paths before middle after) =
    fmap toCycle' <$> (splitLeftPath <|> splitRightPath)
  where
    toCycle' = toCycle splitLeaf splitChildren

    splitLeftPath = case paths of
      RootSplit (RootBefore _ _)     -> Nothing -- there is no left path to split
      RootSplit (RootAfter lPath u)  -> findNodeAlongPath p R lPath <&> \(lSplit,lPath') ->
                Vector2 (extend u (before <> middle) after lSplit)
                        (DecendantSplit u before lPath' (middle <> after))
      PathSplit u lPath rPath        ->
        findNodeAlongPath p R lPath <&> \(lSplit,lPath') ->
          let rPath' = nodeSplitToTree <$> rPath
          in Vector2 (extend u before (middle <> [pathToTree rPath'] <> after) lSplit)
                     (InternalSplit u (Split (Vector2 lPath' rPath') before middle after))

    splitRightPath = case paths of
      RootSplit (RootBefore u rPath) -> findNodeAlongPath p L rPath <&> \(rSplit,rPath') ->
                Vector2 (DecendantSplit u (before <> middle) rPath' after)
                        (extend u (before <> middle) after rSplit)
      RootSplit (RootAfter _ _)      -> Nothing -- there is no right path to split


      PathSplit u lPath rPath        -> findNodeAlongPath p L rPath <&> \(rSplit,rPath') ->
          let lPath' = nodeSplitToTree <$> lPath
          in Vector2 (InternalSplit u (Split (Vector2 lPath' rPath') before middle after))
                     (extend u (before <> [pathToTree lPath'] <> middle) after rSplit)



-- | Extends and reroots the split
extend :: a -> [Tree a] -> [Tree a] -> InitialSplit a (Tree a) -> InitialSplit a (Tree a)
extend u before after = reroot . extendWith u before after . Leaf
