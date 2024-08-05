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
  , planarSeparatorTree


  , NodeSplit(..)
  , Path(..)
  , collectPath
  , foldPath, trimap, trifoldMap
  , findNode
  , pathToTree

  , InitialSplit(..)
  , initialSplit
  , initialSplitToTree
  ) where

import           Control.Applicative
import           Control.Lens ((<&>))
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Monoid (First(..))
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Tree (Tree(..))
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
import           HGeometry.Vector

import           Debug.Trace

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

-- | unsplit the node split into a proper Tree
nodeSplitToTree :: NodeSplit a [Tree a] -> Tree a
nodeSplitToTree = flip nodeSplitToTreeWith []

-- | unsplit the node split into a proper Tree, adding the additional trees in the middle
nodeSplitToTreeWith                                   :: NodeSplit a [Tree a]
                                                      -> [Tree a] -> Tree a
nodeSplitToTreeWith (NodeSplit u before after) middle = Node u $ before <> middle <> after

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

-- | returns the a's left, on, and right of the path
collectPath :: Path a [Tree a] (NodeSplit a [Tree a]) -> NodeSplit [a] [a]
collectPath = foldPath handle (\ns r -> handle ns <> r)
  where
    handle (NodeSplit x before after) = NodeSplit [x] (f before) (f after)
    f = foldMap F.toList

-- | Get the endpoint of the path
endPoint :: Path a trees (NodeSplit a trees) -> a
endPoint = foldPath (\(NodeSplit x _ _) -> x) (\_ l -> l)

----------------------------------------

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

-- | Computes the weight of a ndoesplit
nodeSplitWeight :: (Num w, IsWeight w) => Side -> NodeSplit a [Tree (Weighted w b)] -> w
nodeSplitWeight s (NodeSplit _ before after) = case s of
                                                 L -> weightOf before
                                                 R -> weightOf after


-- | Either left or Right
data Side = L | R deriving (Show,Eq)

-- | Search along a path; we search among the nodees on the path and in the subtrees
-- hanging off the path on the given side.
findNodeAlongPath                   :: (Tree a -> NodeSplit a [Tree a])
                                    -> (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                    -> (a -> Bool)
                                    -> Side -- ^ indicates which subtrees to search
                                    -> Path a [Tree a] (NodeSplit a [Tree a])
                                    -> Maybe ( Cycle' a
                                             , Path a [Tree a] (NodeSplit a [Tree a])
                                             )
findNodeAlongPath splitLeaf' splitChildren' p side = go
  where
    go = \case
      Leaf (NodeSplit u before after)
        | p u       -> Just ( error "findNodeAlongPath; splitting the same leaf?"
                            , Leaf $ splitLeaf' (Node u $ before <> after)
                            )
        | otherwise -> here u before after (RootSplit . flip RootAfter u)
                                           (RootSplit . RootBefore u)

      Path (NodeSplit (u, path) before after)
          | p u       -> Just ( cycle' u path before after
                              , Leaf $ splitLeaf' (Node u $ before <> after)
                              )
          | otherwise -> here u before after (\path' -> PathSplit u path' path)
                                             (PathSplit u path)
                         <|> ( fmap (\path' -> Path $ NodeSplit (u,path') before after)
                              <$> go path)

    here u before after makeL makeR = case side of
        L -> findNode' p before <&> \(NodeSplit path' before' after') ->
                   let path'' = splitLeaf' <$> path'
                   in ( Split (makeL path'') before' after' after
                      , Path $ NodeSplit (u, path'') before' (after' <> after)
                      )
             -- Search on the left; i.e. in the before part
        R -> findNode' p after <&> \(NodeSplit path' before' after') ->
                       let path'' = splitLeaf' <$> path'
                       in ( Split (makeR path'') before before' after'
                          , Path $ NodeSplit (u, path'') (before <> before') after'
                          )

    cycle' u path before after = case splitChildren' u before of
      Nothing -> case splitChildren' u after of
        Nothing                          -> error "toCycle"
        Just (Vector2 middle after') ->
          Split (RootSplit $ RootAfter path u) before middle after'
      Just (Vector2 before' middle) ->
          Split (RootSplit $ RootBefore u path) before' middle after


--------------------------------------------------------------------------------
-- * A Split

-- | Two paths that split the subtree into three subtrees
data Split paths trees = Split paths trees trees trees
  deriving (Show,Eq,Functor,Foldable)

instance Bifunctor Split where
  bimap f g (Split paths as bs cs) = Split (f paths) (g as) (g bs) (g cs )

--------------------------------------------------------------------------------

-- | The actual cycle
type Cycle a trees = Split (CycleSplitPaths a trees) trees
type Cycle' a = Cycle a [Tree a]

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
collectPaths splitChildren' = \case
  RootSplit rs            -> collectRootSplitPath splitChildren' rs
  PathSplit r lPath rPath -> let NodeSplit sepL before  middleL = collectPath lPath
                                 NodeSplit sepR middleR after   = collectPath rPath
                             in ( r : sepL <> sepR
                                , Vector2 (middleL <> middleR) (before <> after)
                                )


cycleSplitPathWeights :: (Num w, IsWeight w) => CycleSplitPaths a [Tree (Weighted w b)]-> w
cycleSplitPathWeights = \case
  RootSplit rs            -> rootSplitWeight rs
  PathSplit _ lPath rPath -> pathWeight L lPath + pathWeight R rPath


-- | the labels of the leaves at which the cyclesplit paths end. If one is a root
-- splitpaht the root comes first.
endPoints :: CycleSplitPaths a [Tree a] -> (a,a)
endPoints = \case
    RootSplit (RootBefore r path) -> (r, endPoint path)
    RootSplit (RootAfter path r)  -> (r, endPoint path)
    PathSplit _ lPath rPath       -> (endPoint lPath, endPoint rPath)

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

-- | Collect on a rootsplitPath
collectRootSplitPath                :: (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                    -> RootSplitPath a [Tree a] -> ([a], Vector 2 [a])
collectRootSplitPath splitChildren' = fromMaybe err . \case
    RootBefore r path -> let (NodeSplit sep inside outside, before, after) = splitPath path in
      splitChildren' r before <&> \(Vector2 before' middle) ->
          (r:sep, Vector2 (flatten middle <> inside) (flatten (before' <> after) <> outside))

    RootAfter path r  -> let (NodeSplit sep outside inside, before, after) = splitPath path in
      splitChildren' r after <&> \(Vector2 middle after') ->
          (r:sep, Vector2 (flatten middle <> inside) (flatten (before <> after') <> outside))
  where
    err = error "collectRootSplitPath: not found!?"
    flatten = foldMap F.toList

    splitPath = \case
      Leaf (NodeSplit _ before after)         -> (mempty,           before,after)
      Path (NodeSplit (_,path') before after) -> (collectPath path', before, after)

-- | computes the weight of the paths hanging off a rootSplit
rootSplitWeight :: (IsWeight w, Num w) => RootSplitPath a [Tree (Weighted w b)] -> w
rootSplitWeight = \case
  RootBefore _ rPath -> pathWeight L rPath
  RootAfter lPath _  -> pathWeight R lPath

--------------------------------------------------------------------------------

-- | Result of the initial split; we find a root split (say when w is a decentant of v)
-- or a proper node split.
data InitialSplit a tree =
    DecendantSplit a [tree] (Path a [tree] tree) [tree]
  | InternalSplit a (Split (Vector 2 (Path a [tree] tree)) [tree])
  deriving (Show,Eq)


-- -- | Annotate the split tree with subtree weights
-- annotateSplitTree :: InitialSplit a (Tree a) -> InitialSplit (Weighted' a) (Tree (Weighted' a))
-- annotateSplitTree = bimap (Weighted 1) annotate

-- | Transform an Initial split into a proper cycle by splitting the leaves (and the root
-- if needed).
toCycle                           :: (tree -> NodeSplit a [tree])
                                  -> (a -> [tree] -> Maybe (Vector 2 [tree]))
                                  -> InitialSplit a tree -> Cycle a [tree]
toCycle splitLeaf' splitChildren' = \case
    InternalSplit v split              -> first (splitLeaves v) split
    DecendantSplit v before path after -> case splitChildren' v before of
      Nothing -> case splitChildren' v after of
        Nothing                          -> error "toCycle"
        Just (Vector2 middle after') ->
          Split (RootSplit $ RootAfter (splitLeaf' <$> path) v) before middle after'
      Just (Vector2 before' middle) ->
          Split (RootSplit $ RootBefore v (splitLeaf' <$> path)) before' middle after
  where
    splitLeaves r (Vector2 lPath rPath) =
      PathSplit r (splitLeaf' <$> lPath) (splitLeaf' <$> rPath)

-- | Annotates a cycle with the subtree weights.
annotateCycle :: Cycle' a -> Cycle' (Weighted' a)
annotateCycle = bimap (bimap (Weighted 1) (fmap annotate)) (fmap annotate)

-- | Convert the initial split back into a tree.
initialSplitToTree :: InitialSplit a (Tree a) -> Tree a
initialSplitToTree = \case
  DecendantSplit u before path after -> Node u (before <> [pathToTree path] <> after)
  InternalSplit u (Split (Vector2 lPath rPath) before middle after) ->
    Node u (before <> [pathToTree lPath] <> middle <> [pathToTree rPath] <> after)

--------------------------------------------------------------------------------

-- | the prefix should become part of the outside; in particular we put them on right of
-- the righstmost input leaf
splitTree                             :: Eq a
                                      => (Tree a -> NodeSplit a [Tree a])
                                      -> (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                      -> (a,a)
                                      -> Tree a
                                      -> Cycle' a
splitTree splitLeaf' splitChildren' e = toCycle splitLeaf' splitChildren' . initialSplit e

-- | Computes the initial split.
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
                                                     in internalSplit u paths before' middle after
            Nothing -> case findW after of
              Just (NodeSplit pathW middle after') -> let paths = Vector2 pathV pathW
                                                      in internalSplit u paths before middle after'
              Nothing -> go pathV <&> \path' -> Path (NodeSplit (u, path') before after)

    findW = findNode' (== w)

    internalSplit u paths before middle after =
      Just . Leaf . InternalSplit u $ Split paths before middle after

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


type Weight = Int
type Weighted' = Weighted Weight

-- | Tries to split the cycle at the given node
splitCycleAt                                     :: Show a =>
                                                    (Tree a -> NodeSplit a [Tree a])
                                                 -> (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                                                 -> (a -> Bool)
                                                 -> Cycle' a
                                                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAt splitLeaf' splitChildren' p (Split paths before inside after)
  | traceShow ("splitCycleAt",paths,before,inside,after) False = undefined
  | otherwise
  =
    splitInterior <|> splitCycleAtPath splitLeaf' splitChildren' p paths before inside after
  where
    splitInterior = traceShowWith ("splitInterior",) $
      constructCycles . first (fmap splitLeaf') <$> findNode' p inside

    constructCycles (NodeSplit path before' after') = Vector2
        (Split lPaths (before <> lBefore)            before' (after' <> lAfter <> after))
        (Split rPaths (before <> rBefore <> before') after'  (rAfter <> after))
      where
        ( (lPaths, lBefore, lAfter), (rPaths, rBefore, rAfter) ) = case paths of
          RootSplit (RootBefore r rPath) ->
            ( (RootSplit (RootBefore r path), [],                  [pathToTree' rPath])
            , (PathSplit r path rPath,        [],                  []                )
            )
          RootSplit (RootAfter lPath r)  ->
            ( (PathSplit r lPath path,        [],                  [])
            , (RootSplit (RootAfter path r),  [pathToTree' lPath], [])
            )
          PathSplit r lPath rPath        ->
            ( (PathSplit r lPath path,        [],                  [pathToTree' rPath])
            , (PathSplit r path rPath,        [pathToTree' lPath], [])
            )

-- | Given a predicate that indicates the node we are trying to find, looks in the
-- subtrees hanging off of the paths/spines if we can find it, and returns the two cyclces
-- we get by splitting the trees there.
splitCycleAtPath :: forall a. Show a =>
                    (Tree a -> NodeSplit a [Tree a])
                 -> (a -> [Tree a] -> Maybe (Vector 2 [Tree a]))
                 -> (a -> Bool)
                 -> CycleSplitPaths a [Tree a]
                 -> [Tree a] -> [Tree a] -> [Tree a]
                 -> Maybe (Vector 2 (Cycle' a))
splitCycleAtPath splitLeaf' splitChildren' p paths before inside after = case paths of
   RootSplit (RootAfter lPath r)  ->
     (combineR (RootSplit . flip RootAfter r)) <$>
           findNodeAlongPath splitLeaf' splitChildren' p R lPath
   RootSplit (RootBefore r rPath) ->
     (combineL (RootSplit . RootBefore r))     <$>
           findNodeAlongPath splitLeaf' splitChildren' p L rPath
   PathSplit r lPath rPath              -> traceShowWith ("splitCycleAtPath, pathsplit",r,lPath,rPath,)
     (traceShowWith ("combineL",) $
       combineL (\lPath' -> PathSplit r lPath' rPath)  <$>
           findNodeAlongPath splitLeaf' splitChildren' p R lPath)
     <|>
     (traceShowWith ("combineR",) $
       combineR (PathSplit r lPath)                    <$>
           findNodeAlongPath splitLeaf' splitChildren' p L rPath)
  where
    -- creates a new split, where the new split is on the left, and the updated remainder
    -- of the current split is on the right.
    combineL                 :: ( Path a [Tree a] (NodeSplit a [Tree a]) ->
                                                            CycleSplitPaths a [Tree a])
                             -> (Cycle' a, Path a [Tree a] (NodeSplit a [Tree a]))
                             -> Vector 2 (Cycle' a)
    combineL f (split,path') = Vector2 split (mkSplit . f $ path')
    -- creates a new split, where the new split is on the right
    combineR                 :: ( Path a [Tree a] (NodeSplit a [Tree a]) ->
                                                            CycleSplitPaths a [Tree a])
                             -> (Cycle' a, Path a [Tree a] (NodeSplit a [Tree a]))
                             -> Vector 2 (Cycle' a)
    combineR f (split, path') = Vector2 (mkSplit . f $ path') split
    mkSplit paths' = Split paths' before inside after

--------------------------------------------------------------------------------

-- | Construct a balanced separator based on the given tree. The separator hass at most
-- 2n/3 vertices on either side, and consists of at most 2h nodes (where h) is the height
-- of the tree.
planarSeparatorTree                     :: forall k v e.
                                           ( Ord k
                                           , Show k
                                           )
                                        => Weight -- ^ maximum allowed weight on the heavy
                                                  -- side; i.e. the 2n/3.
                                        -> PlaneGraph k v e -> Tree k -> ([k], Vector 2 [k])
planarSeparatorTree allowedWeight gr tr = go initialCycle
  where
    e@(_,w)      = traceShowWith ("(v,w)",)
                 $ Set.findMin $ graphEdges gr `Set.difference` treeEdges tr
    initialCycle = traceShowWith ("initialCycle",) $
      makeInsideHeaviest
      . traceShowWith ("beforeMakeHeaviest",)
      . annotateCycle
      . splitTree splitLeaf0 splitChildren0 e $ tr

    splitLeaf0     = splitLeaf     id gr e
    splitChildren0 = splitChildren id gr (== w)
      -- if e = (v,w), then we are splitting v's children

    splitLeaf' = splitLeaf getValue gr e

    -- compute the actual separator
    go :: Cycle' (Weighted' k) -> ([k], Vector 2 [k])
    go cycle'
      | interiorWeight cycle' <= allowedWeight = toSeparator gr cycle'
      | otherwise                              =
          case getFirst $ foldMap splitCycle (commonNeighbours e gr) of
            Nothing                 -> error "planarSeparatorTree: impossible"
            Just (Weighted w' cycle'')
              | w' <= allowedWeight -> traceShowWith ("go otherwise",cycle'',) $
                toSeparator gr cycle''
              | otherwise           -> go cycle''
      where
        splitCycle   :: k -> First (Weighted' (Cycle' (Weighted' k)))
        splitCycle u = First . fmap ( F.maximumBy (comparing getWeight)
                                    . fmap (\c -> Weighted (interiorWeight c) c))
                     $ traceShowWith ("splitCycle",u,)
                     $ splitCycleAt splitLeaf' splitChildren' p cycle'
          where
            p = (== u) . getValue
            splitChildren' = splitChildren getValue gr (== u) . getValue
    -- FIXME: I guess I should'n't pass e to splitLeaf' either !

-- | Compute the weight on the inside of the cycle
interiorWeight                          :: (Num w, IsWeight w) => Cycle' (Weighted w a) -> w
interiorWeight (Split paths _ inside _) = cycleSplitPathWeights paths + weightOf inside

-- | Turn the weighted cycle into an actual separator.
toSeparator    :: Ord k => PlaneGraph k v e -> Cycle' (Weighted' k) -> ([k], Vector 2 [k])
toSeparator gr (Split paths before middle after) =
    (sep, Vector2 (inside <> toList' middle) (outside <> toList' before <> toList' after))
  where
    (sep, Vector2 inside outside) = bimap getV (fmap getV) $ collectPaths splitChildren' paths
    toList' = getV . foldMap F.toList
    getV = fmap getValue

    (_,w) = endPoints paths
    splitChildren' = splitChildren getValue gr (== getValue w) . getValue

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Given the graph, an edge (v,w) in the graph, and a tree rooted at weither v or w
-- split the Tree
splitLeaf                           :: Ord k
                                    => (a -> k)
                                    -> PlaneGraph k v e -> (k,k)
                                    -> Tree a
                                    -> NodeSplit a [Tree a]
splitLeaf f gr (v',w') (Node u chs) =
    case splitChildren f gr (if f u == v' then (w'==) else (v'==)) (f u) chs of
      Nothing                     -> error "splitLeaf: absurd. edge not found!?"
      Just (Vector2 before after) -> NodeSplit u before after

-- | Split a list of children.
splitChildren            :: Ord k
                           => (a -> k)
                           ->  PlaneGraph k v e
                           -> (k -> Bool) -- ^ the node that we are searching for/splitting with
                           -> k -- ^ the node whose children we are splitting
                           -> [Tree a] -- ^ the children of the root
                           -> Maybe (Vector 2 [Tree a])
splitChildren f gr p v chs = case List.break (p . snd) adjacencies of
    (before, _:after) -> Just $ Vector2 (mapMaybe fst before) (mapMaybe fst after)
    _                 -> Nothing
  where
    adjacencies = annotateSubSet (f . root) chs
                $ maybe [] (Map.elems . fst) (Map.lookup v gr)


-- | Given a tagging function, a subset, and the full set, tag the elements in the full set
-- with whether or not they are present in the subset. Both sets should be sorted.
annotateSubSet   :: Eq b => (a -> b) -> [a] -> [b] -> [(Maybe a,b)]
annotateSubSet f = go
  where
    go []            fullSet = map (Nothing,) fullSet
    go subSet@(x:xs) (y:ys)
      | f x == y                         = (Just x,  y) : go xs     ys
      | otherwise                        = (Nothing, y) : go subSet ys
    go _             []      = [] -- this case should not really happen if the first is a subset
