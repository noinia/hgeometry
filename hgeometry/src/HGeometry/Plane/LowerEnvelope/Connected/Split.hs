--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Split
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

  , module HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
  , module HGeometry.Plane.LowerEnvelope.Connected.Separator.InitialSplit
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
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.InitialSplit
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Cycle
import           HGeometry.Vector

import           Debug.Trace

--------------------------------------------------------------------------------
-- * Paths


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * Computes a planar separator

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
