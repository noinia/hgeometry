{-# LANGUAGE  UndecidableInstances  #-}
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
module HGeometry.Plane.LowerEnvelope.Connected.Separator
  ( planarSeparator
  ) where

import           Control.Lens ((^..),(^?), asIndex)
import qualified Data.Foldable as F
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Foldable.Util ()
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Vector
import           Hiraffe.AdjacencyListRep.Map
import           Hiraffe.BFS.Pure
import           Hiraffe.Graph.Class

--------------------------------------------------------------------------------

data LevelInfo v = Level { levelIndex  :: {-# UNPACK #-}!Int
                         , levelSize   :: {-# UNPACK #-}!Int
                         -- ^ size of this level
                         , accumSize :: {-# UNPACK #-}!Int
                         -- ^ size of the prefix up to this level
                         , levelVertices :: [v]
                         }
                 deriving (Show,Eq,Foldable)

sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

-- planarSeparator    :: PlanarGraph_ planarGraph
--                    => planarGraph
--                    -> ([VertexIx planarGraph], Vector 2 [VertexIx planarGraph])

type Separator k = ([k],Vector 2 [k])


-- | Extracts the maximum from a non-empty list using the given ordering
viewMaximumBy              :: (a -> a -> Ordering) -> NonEmpty a -> (a, [a])
viewMaximumBy cmp (x0:|xs) = foldr (\x (m,rest) -> case x `cmp` m of
                                                     LT -> (m, x:rest)
                                                     EQ -> (m, x:rest)
                                                     GT -> (x, m:rest)

                                   ) (x0, []) xs

-- | Returns a pair (separator, Vector2 verticesSubGraphA verticesSubGraphB)
-- so that
--
-- 1) there are no edges connecting subGraph A and subgraph B,
-- 2) the size of the separator is at most sqrt(n).
-- 3) the vertex sets of A and B have weight at most 2/3 the total weight
planarSeparator    :: Ord k => PlaneGraph' k v e -> Separator k
planarSeparator gr = case viewMaximumBy (comparing snd) trees of
    ((tr,m),rest)
      | m <= twoThirds -> groupComponents
      | otherwise      -> planarSeparator' tr m -- we should also add the remaining vertices
  where
    trees = (\t -> (t, length t)) <$> bff gr
    n     = sum $ fmap snd trees
    half = n `div` 2
    twoThirds = 2 * (n `div` 3)

    groupComponents = undefined

    planarSeparator' tr _ = case List.break (\lvl -> accumSize lvl < half) lvls of
        (_,    [])          -> ([], Vector2 (F.toList tr) [])
                                 -- somehow we have too little weight;
        (pref, (l1 : suff)) -> planarSeparatorTree gr tr'
          where
            k      = accumSize l1
            p  lvl = levelSize lvl + 2*(levelIndex l1  - levelIndex lvl)    <= 2 * sqrt' k
            p' lvl = levelSize lvl + 2*(levelIndex lvl - levelIndex l1 - 1) <= 2 * sqrt' (n-k)

            l0 = findR     p  (pref <> [l1])
            l2 = List.find p' suff
            tr' = trim l0 l2 tr


            -- sep       = undefined
            -- verticesA = undefined
            -- verticesB = undefined
      where
            -- compute the levels, their sizes, and the sum of their sizes
        (_, lvls) = List.mapAccumL (\(Vector2 i acc) lvl ->
                                       let m    = length lvl
                                           acc' = acc + m
                                       in ( Vector2 (i+1) acc', Level i m acc' lvl)
                                   ) (Vector2 0 0) $ levels tr


-- | contracts the plane graph so that we get a spanning tree of diameter at most sqrt(n).
contract :: PlaneGraph' k v e -> Tree k -> (PlaneGraph' k v e, Tree k)
contract = undefined

trim _ _ tr = tr
-- TODO:

-- | Given a spanning tree of the graph that has diameter r, compute
-- a separator of size at most 2r+1
planarSeparatorTree       :: Ord k => PlaneGraph' k v e -> Tree k -> Separator k
planarSeparatorTree gr tr = (sep, foldMap F.toList <$> trees)
  -- FIXME: continue searching
  where
    e = Set.findMin $ graphEdges gr `Set.difference` treeEdges tr
    (sep, trees) = fromSplitTree . splitLeaf gr e $ splitTree e tr

--------------------------------------------------------------------------------
-- * Spliting the tree

-- data SplitTree l a = RootSplit l [Tree a] (Path l a) [Tree a]
--                    | Prefix (Path (a, Split l a) a)
--                    deriving (Show,Eq)
-- -- still not quite right, since now we can't represent rotosplits lower than the root .

newtype SplitTree a l = SplitTree (Path a (Split a l))
  deriving (Show,Eq,Functor)

-- | A path in the tree that ends at a "leaf" in which we store something of type l
newtype Path a l = MkPath (NonEmpty (PathNode a l))
  deriving (Show,Eq,Functor)

data PathNode a l = PathLeaf l
                  | PathNode a [Tree a] [Tree a]
                  deriving (Show,Eq,Functor)

pattern Leaf   :: l -> Path a l
pattern Leaf l = MkPath (PathLeaf l :| [])

(<|) :: PathNode a l -> Path a l -> Path a l
n <| (MkPath path) = MkPath $ n NonEmpty.<| path

pattern Path                     :: a -> [Tree a] -> Path a l -> [Tree a] -> Path a l
pattern Path u before path after <- (unconsPath -> Just (u, before, after, path))
  where
    Path u before path after = PathNode u before after <| path


unconsPath :: Path a l -> Maybe (a, [Tree a], [Tree a], Path a l)
unconsPath = \case
  MkPath (PathNode u before after :| path') -> (u,before,after,) . MkPath
                                           <$> NonEmpty.nonEmpty path'
  _                                         -> Nothing
{-# COMPLETE Leaf, Path #-}


-- | The split node where the two paths diverge
data Split a l =
    RootSplit l -- ^ apparently root is the split we are looking for.
              [Tree a] (Path a l) [Tree a]
  | NodeSplit a -- ^ label of the node we are splitting
              [Tree a] -- ^ children before the left path
              (Path a l)
              -- ^ the value stored at the left node (i.e. the leaf) we argoing to,
              -- and the pato that goes there.
              [Tree a] -- ^ middle nodes
              (Path a l)
              -- ^ the value stored at the right node we argoing to, and the pato that
              -- goes there.
              [Tree a]
  deriving (Show,Eq,Functor)

-- | Given an non-tree edge (v,w), split the tree usign the root to v,w paths
splitTree     :: Eq a => (a,a) -> Tree a -> SplitTree a (Tree a)
splitTree e t = case splitTree' e t of
  Both split -> split
  _          -> error "splitTree: absurd, didn't find both endpoints"

data ResultF a b = NotFound
                 | Single a
                 | Both b
                 deriving (Show,Eq,Functor)

type Result a = ResultF (VW, Path a (Tree a) ) (SplitTree a (Tree a))

data VW = V | W

other     :: p -> p -> VW -> p
other v w = \case
  V -> w
  W -> v

data Loc a b = Here a | There b deriving (Show,Eq)

-- | Implementation of splitTree; i.e. tries to find both endpoints of the given edge.
splitTree'       :: Eq a => (a,a) -> Tree a -> Result a
splitTree' (v,w) = fmap SplitTree . go
  where
    -- Handle the cases that we find one of the elemtns (identified by 'found') here.
    here found tr chs = case findNodes w chs of
      Nothing                    -> Single (found, Leaf tr)
      Just (before, after, path) -> Both . Leaf $ RootSplit tr before path after

    go tr@(Node u chs)
      | u == v    = here V tr chs
      | u == w    = here W tr chs
      | otherwise = case foldr process (NotFound, []) chs of
          (NotFound, _)                     -> NotFound
          (Single (middle, (x,path)),after) -> Single (x, PathNode u middle after <| path)
          (Both (before, both'), after)     -> Both $ case both' of
                Here  (lp,middle,rp)  -> Leaf $ (NodeSplit u before lp middle rp after)
                There path            -> PathNode u before after <| path

    process ch@(Node u chs) = \case
      (NotFound, after)            -> case go ch of
        NotFound          -> (NotFound,              ch:after)
        Single rightPath  -> (Single ([], rightPath),   after)
        Both split        -> (Both   ([], There split), after)

      (Single (middle, path@(x, rightPath)), after)
        | other v w x == u -> (Both ([], Here (Leaf ch, middle, rightPath)), after)
        | otherwise        -> case pathNode u <$> findNodes (other v w x) chs of
            Nothing       -> (Single (ch:middle,path),                       after)
            Just leftPath -> (Both ([], Here (leftPath, middle, rightPath)), after)

      (Both   (before, split),     after) -> (Both (ch:before, split), after)

-- | Search for a given element in a bunch of trees. Returns the path towards
-- the node if we find it.
findNodes   :: Eq a => a  -> [Tree a]  -> Maybe ([Tree a], [Tree a], Path a (Tree a))
findNodes v = go
  where
    go chs = case foldr process (Nothing, []) chs of
               (Nothing, _)                 -> Nothing
               (Just (before, path), after) -> Just (before, after, path)

    process ch = \case
      (Nothing,             after) -> case findNode' ch of
        Nothing   -> (Nothing,        ch:after)
        Just path -> (Just ([],path),    after)
      (Just (before, path), after) -> (Just (ch:before, path), after)

    findNode' t@(Node u chs)
      | u == v    = Just (Leaf t)
      | otherwise = pathNode u <$> go chs

-- | Smart constructor for producign a pathNode
pathNode                        :: a -> ([Tree a], [Tree a], Path a l) -> Path a l
pathNode u (before, after, path) = PathNode u before after <| path

----------------------------------------

type EndPoint a = (a, [Tree a], [Tree a])

-- | Split the leaf of the path
splitLeaf            :: Ord k
                     => PlaneGraph' k v e
                     -> (k,k) -> SplitTree k (Tree k) -> SplitTree k (EndPoint k)
splitLeaf gr (v',w') = fmap $ \(Node u chs) -> split u chs (if u == v' then w' else v')
  where
    split v chs w = case List.break (hasEdge v) chs of
                      (before, _:after) -> (v, before, after)
                      _                 -> error "splitLeaf: absurd. edge not found!?"
    hasEdge v w = isJust $ gr^?edgeAt (v, root w)
      -- maybe False (F.elem (root w) . fst) $ gr^?vertexAt v
    -- note: this is linear in the number of edges out of v

-- | Turn the split tree into a separator, and the trees inside the cycle, and outside the
-- separator.
fromSplitTree               :: Eq a => SplitTree a (EndPoint a) -> ([a],Vector 2 [Tree a])
fromSplitTree (SplitTree t) = go t
  where
    go = \case
      Leaf split               -> fromSplit split
      Path u before path after -> let (sep,Vector2 inside outside) = go path
                                  in (u : sep,Vector2 inside (before <> outside <> after))

-- | Handling a split node
fromSplit :: Eq a => Split a (EndPoint a) -> ([a],Vector 2 [Tree a])
fromSplit = \case
  RootSplit (v,beforeV,afterV) _ path _ -> case path of
    Leaf (_,_,_)     -> error "w is a child of v, that shouldn't really happen"
    Path u _ path' _ -> case List.break ((== u) . root) beforeV of
        -- edge vw lies after the path from v via u to w
        (before, _:insideV)  -> (v : u : sep, Vector2 inside outside)
          where
            (sep, Vector2 insideU beforeU) = fromPath After path'
            inside  = insideU <> insideV
            outside = before <> beforeU <> afterV
        -- ede vw lies before the path from v via u to w
        _                     -> case List.break ((== u) . root) afterV of
          (middle, _:afterU) -> (v : u : sep, Vector2 inside outside)
            where
              (sep, Vector2 insideU afterW) = fromPath Before path'
              inside  = middle <> insideU
              outside = beforeV <> afterW <> afterU
          _                   -> error "fromSplit. Rootsplit (v,w) not found"
  NodeSplit u before lp middle rp after -> (u : lSep <> rSep, Vector2 inside outside)
    where
    (lSep, Vector2 lInside lOutside) = fromPath After  lp
    (rSep, Vector2 rInside rOutside) = fromPath Before rp

    inside  = lInside  <> rInside
    outside = before <> lOutside <> rOutside <> after

data Side = Before | After deriving (Show,Eq)

-- | And handling the path. The side indicates which side is the inside.
fromPath       :: Side -> Path a (EndPoint a) -> ([a],Vector 2 [Tree a])
fromPath sel = go
  where
    go = \case
      Leaf (v, before, after) -> case sel of
                                   Before -> ([v], Vector2 before after)
                                   After  -> ([v], Vector2 after before)
      Path u before path after    -> let (sep, Vector2 inside outside) = go path
                                     in case sel of
          Before -> (u : sep, Vector2 (before <> inside) (after  <> outside))
          After  -> (u : sep, Vector2 (after  <> inside) (before <> outside))

class IsWeight w where
  data Weighted w :: Type -> Type
  getWeight :: Weighted w a -> w

instance IsWeight Int where
  data Weighted Int a = Weighted {-#UNPACK#-}!Int a deriving (Show,Eq,Functor)
  getWeight (Weighted w _) = w

mkSeparator       :: (Num w, IsWeight w)
                => (a,a) -> Tree (Weighted w a) -> Vector 2 (Tree (Weighted w a))
mkSeparator (v,w) = go
  where
    go (Node u chs) = undefined




costs :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
costs = sum . map (getWeight . root)



-- | Annotate tht tree with the size of the subtrees
annotate              :: Tree k -> Tree (Weighted Int k)
annotate (Node v chs) = let chs' = map annotate chs
                        in Node (Weighted (1 + costs chs') v) chs'

graphEdges   :: Ord k => PlaneGraph' k v e -> Set (k,k)
graphEdges gr = Set.fromList [ (u,v) | (u,v) <- gr^..edges.asIndex, u <= v ]


  -- Map.foldMapWithKey (\u (es,_) -> Set.fromList [ (u,v) | v <- Map.elems es, u <= v])

treeEdges              :: Ord k => Tree k -> Set (k,k)
treeEdges (Node u chs) = Set.fromList [ orient (u,v) | Node v _ <- chs ]
                      <> foldMap treeEdges chs
  where
    orient (a,b) = if a <= b then (a,b) else (b,a)

root            :: Tree a -> a
root (Node v _) = v

--------------------------------------------------------------------------------

-- | Find the last element matching some predicate
findR   :: (a -> Bool) -> [a] -> Maybe a
findR p = List.find p . reverse



-- if the input graph is connected, are subgraph A and SubGraphB then connected?
-- I don't think so; in particular; the "outer layers", so Graph B may be disconnected I guess.


-- data Separators planarGraph =


-- planarSeparators :: PlanarGraph_ planarGraph
--                  => planarGraph -> Tree
