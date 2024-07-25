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

import qualified Data.Foldable as F
import           Data.Kind (Type)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * BFS

-- | Computes a breath first forest
-- (O(n \log n))
bff    :: Ord k => PlaneGraph k v e -> [Tree k]
bff gr = go (Map.keysSet gr)
  where
    go remaining = case Set.minView remaining of
      Nothing              -> []
      Just (v, remaining') -> let (remaining'', t) = bfs gr v remaining'
                              in t : go remaining''

-- | Turn the map into a tree.
toTree   :: Ord k => Map k [k] -> k -> Tree k
toTree m = go
  where
    go s = Node s $ map go (fromMaybe [] $ Map.lookup s m)

-- | BFS from the given starting vertex, and the set of still-to-visit vertices.
-- returns the remaining still-to-visit vertices and the tree.
bfs      :: Ord k => PlaneGraph k v e -> k -> Set k -> (Set k, Tree k)
bfs gr s = fmap (flip toTree s) . bfs' [s]
  where
    bfs' lvl remaining = foldr visit (remaining, Map.empty) lvl

    visit v (remaining, m) = let chs = filter (flip Set.member remaining) $ neighs v
                             in (foldr Set.delete remaining chs, Map.insert v chs m)
    neighs v = maybe [] (Map.elems . fst) $ Map.lookup v gr

--------------------------------------------------------------------------------




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

-- | Returns a pair (separator, Vector2 verticesSubGraphA verticesSubGraphB)
-- so that
--
-- 1) there are no edges connecting subGraph A and subgraph B,
-- 2) the size of the separator is at most sqrt(n).
-- 3) the vertex sets of A and B have weight at most 2/3 the total weight
planarSeparator    :: Ord k => PlaneGraph k v e -> Separator k
planarSeparator gr = case trees of
    []                 -> ([],Vector2 [] [])
    ((tr,m):rest)
      | m <= twoThirds -> groupComponents
      | otherwise      -> planarSeparator' tr m -- we should also add the remaining vertices
  where
    trees = List.sortOn snd . map (\t -> (t, length t)) $ bff gr
    n     = sum $ map snd trees
    half = n `div` 2
    twoThirds = 2 * (n `div` 3)

    groupComponents = undefined

    planarSeparator' tr _ = case List.break (\lvl -> accumSize lvl < half) lvls of
        (_,    [])          -> ([], Vector2 (F.toList tr) [])
                                 -- somehow we have too little weight;
        (pref, (l1 : suff)) -> (sep, Vector2 verticesA verticesB)
          where
            k      = accumSize l1
            p  lvl = levelSize lvl + 2*(levelIndex l1  - levelIndex lvl)    <= 2 * sqrt' k
            p' lvl = levelSize lvl + 2*(levelIndex lvl - levelIndex l1 - 1) <= 2 * sqrt' (n-k)

            l0 = findR     p  (pref <> [l1])
            l2 = List.find p' suff
            tr' = trim l0 l2 tr


            sep       = undefined
            verticesA = undefined
            verticesB = undefined
      where
            -- compute the levels, their sizes, and the sum of their sizes
        (_, lvls) = List.mapAccumL (\(Vector2 i acc) lvl ->
                                       let m    = length lvl
                                           acc' = acc + m
                                       in ( Vector2 (i+1) acc', Level i m acc' lvl)
                                   ) (Vector2 0 0) $ levels tr

        lZero = 0
-- | contracts the plane graph so that we get a spanning tree of diameter at most sqrt(n).
contract :: PlaneGraph k v e -> Tree k -> (PlaneGraph k v e, Tree k)
contract = undefined

trim _ _ tr = tr
-- TODO:

-- | Given a spanning tree of the graph that has diameter r, compute
-- a separator of size at most 2r+1
planarSeparatorTree       :: Ord k => PlaneGraph k v e -> Tree k -> Separator k
planarSeparatorTree gr tr = undefined
  where
    (v,w) = Set.findMin $ graphEdges gr `Set.difference` treeEdges tr


--------------------------------------------------------------------------------
-- * spliting the tree

-- | A path in the tree that ends at a "leaf" in which we store something of type l
type Path l a = NonEmpty (PathNode l a)
data PathNode l a = PathLeaf l
                  | PathNode a [Tree a] [Tree a]
                  deriving (Show,Eq)

pattern Leaf   :: l -> Path l a
pattern Leaf l = PathLeaf l :| []

pattern Path                     :: a -> [Tree a] -> Path l a -> [Tree a] -> Path l a
pattern Path u before path after <- (unconsPath -> Just (PathNode u before after, path))
  where
    Path u before path after = PathNode u before after NonEmpty.<| path

unconsPath :: Path l a -> Maybe (PathNode l a, Path l a)
unconsPath = \case
  n@(PathNode _ _ _) :| path' -> (n,) <$> NonEmpty.nonEmpty path'
  _                           -> Nothing
{-# COMPLETE Leaf, Path #-}


type EndPoint a = Vector 2 [Tree a]

-- | The split node where the two paths diverge
data Split a = Split a    -- ^ label of this node
                     [Tree a] -- ^ children before the left path
                     (a, Path (EndPoint a) a)
                     -- ^ the value stored at the left node (i.e. the leaf) we argoing to,
                     -- and the pato that goes there.
                     [Tree a] -- ^ middle nodes
                     (a, Path (EndPoint a) a)
                     -- ^ the value stored at the right node we argoing to, and the pato that
                     -- goes there.
                     [Tree a]
  deriving (Show,Eq)

type SplitTree a = Path (Split a) a

type ParentMap a = Map a (Tree a, Int)

parentMap                :: Ord a => Tree a -> ParentMap a
parentMap u@(Node _ chs) = let f i ch = (root ch, (u, i))
                           in (Map.fromAscList $ zipWith f [0..] chs) <> foldMap parentMap chs

-- Path from the root to the given node
pathToRoot      :: Ord a => Tree a -> ParentMap a -> a -> Path (Tree a) a
pathToRoot tr m = go []
  where
    go path v = case Map.lookup v m of
                  Nothing              -> fromMaybe (Leaf tr) $ NonEmpty.nonEmpty path
                  Just (Node p chs, i) -> case List.splitAt (i-1) chs of
                    (before, v':after) -> let path' = PathNode p before after : case path of
                                                        [] -> [PathLeaf v']
                                                        _  -> path
                                          in go path' p
                    _                  -> error "pathToRoot: absurd"




-- splitTree         :: Ord a => (a,a) -> Tree a -> SplitTree a
-- splitTree (v,w) tr =
--   where
--     m = parentMap tr

--   findNode $ \(Node u chs)

  -- go
  -- where
  --   go

-- | Try to find the node indicated by the "predicate" function (i.e. the function is
-- supposed to return Just v if we find the node v, and Nothing otherwise).
findNode   :: (Tree a -> Maybe l)
           -> Tree a -> Maybe (a, Path l a)
findNode f = findNode'
  where
    findNode' u@(Node u' chs) = case f u of
      Just l  -> Just $ (u', Leaf l)
      Nothing -> case foldr g (Nothing, []) chs of
        (Nothing, _)                    -> Nothing -- not found
        (Just (before,(x,path)), after) -> Just $ (x, Path u' before path after)

    g v = \case
      (Nothing, after)             -> case findNode' v of
                                        Nothing   -> (Nothing,        v:after)
                                        Just path -> (Just ([],path),   after)
      (Just (before,path), after)  -> (Just (v:before, path),after)


----------------------------------------

-- | Turn the split tree into a separator, and the trees inside the cycle, and outside the
-- separator.
fromSplitTree :: SplitTree a -> ([a],Vector 2 [Tree a])
fromSplitTree = \case
  Leaf split               -> fromSplit split
  Path u before path after -> let (sep,Vector2 inside outside) = fromSplitTree path
                              in (u : sep,Vector2 inside (before <> outside <> after))

-- | Handling a split node
fromSplit                                             :: Split a -> ([a],Vector 2 [Tree a])
fromSplit (Split u before (v,lp) middle (w,rp) after) =
    ([u,v] <> lSep <> [w] <> rSep, Vector2 inside outside)
  where
    (lSep, Vector2 lInside lOutside) = fromPath After  lp
    (rSep, Vector2 rInside rOutside) = fromPath Before rp

    inside  = lInside  <> rInside
    outside = before <> lOutside <> rOutside <> after

data Select = Before | After deriving (Show,Eq)

-- | And handling the path
fromPath     :: Select
             -> Path (EndPoint a) a -> ([a],Vector 2 [Tree a])
fromPath sel = go
  where
    go = \case
      Leaf (Vector2 before after) -> case sel of
          Before -> ([], Vector2 before after)
          After  -> ([], Vector2 after before)
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

graphEdges :: Ord k => PlaneGraph k v e -> Set (k,k)
graphEdges = Map.foldMapWithKey (\u (es,_) -> Set.fromList [ (u,v) | v <- Map.elems es, u <= v])

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
