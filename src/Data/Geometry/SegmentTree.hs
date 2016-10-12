{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.SegmentTree where

import Data.Semigroup
import Control.Lens
import           Data.List.NonEmpty (NonEmpty)
import Data.Ext
import Data.BinaryTree
import Data.Range
import Data.Geometry.Interval
import Data.Geometry.Properties

--------------------------------------------------------------------------------


data NodeData v r = NodeData { _splitPoint :: r
                             , _range      :: Range r
                             , _assoc      :: v
                             }
                    deriving (Show,Eq,Functor)
makeLenses ''NodeData



--------------------------------------------------------------------------------

-- | Segemnet tree on a Fixed set of endpoints
newtype SegmentTree v r = SegmentTree { _unTee :: BinLeafTree (NodeData v r) r }
                          deriving (Show,Eq)
makeLenses ''SegmentTree



createTree       :: NonEmpty r -> v -> SegmentTree v r
createTree pts v = SegmentTree . foldUpData f g . fmap _unElem
                 . asBalancedBinLeafTree $ pts
  where
    f l _ r = let m  = l^.range.upper.unEndPoint
                  ll = l^.range.lower
                  rr = r^.range.upper
              in NodeData m (Range ll rr) v
    g p     = NodeData p (ClosedRange p p) v


search   :: (Ord r, Monoid v) => r -> SegmentTree v r -> v
search x = mconcat . stab x

-- | Returns the associated values of the nodes on the search path to x
stab                   :: Ord r => r -> SegmentTree v r -> [v]
stab x (SegmentTree t) = stabRoot t
  where
    stabRoot (Leaf _) = []
    stabRoot (Node l (NodeData m rr v) r) = case (x `inRange` rr, x <= m) of
      (False,_)   -> []
      (True,True) -> v : stab' l
      _           -> v : stab' r

    stab' (Leaf _)                                = []
    stab' (Node l (NodeData m _ v) r) | x <= m    = v : stab' l
                                      | otherwise = v : stab' r


class Measured v i => Assoc v i where
  insertAssoc :: i -> v -> v
  deleteAssoc :: i -> v -> v

-- instance Measured [Interval p r] (Interval p r) where
--   insertAssoc i = (i:)
--   -- deleteAssoc i xs = x \\ i



class IntervalLike i where
  toRange :: i -> Range (NumType i)

instance IntervalLike (Interval p r) where
  toRange = fmap (^.core) . _unInterval




-- | Pre: the interval should have one of the endpoints on which the tree is built.
insert                   :: (Assoc v i, NumType i ~ r, Ord r, IntervalLike i)
                         => i -> SegmentTree v r -> SegmentTree v r
insert i (SegmentTree t) = SegmentTree $ insert' False t
  where
    ri@(Range' a b) = toRange i

    newND coversParent coversMe nd
      | coversParent && coversMe = nd&assoc %~ insertAssoc i
      | otherwise                = nd

    -- pre: b > rr^.upper
    insertL _            l@(Leaf _) = l
    insertL coversParent (Node l nd@(NodeData m rr _) r)
      | a <= m    = let cp  = ri `covers` rr
                    in Node (insertL cp l) (newND coversParent cp nd) r
      | otherwise = let cp  = ri `covers` rr
                    in Node l (newND coversParent cp nd) (insertL cp r)


    -- pre: a < rr^.lower
    insertR _            l@(Leaf _) = l
    insertR coversParent (Node l nd@(NodeData m rr _) r)
      | m <= b    = let cp  = ri `covers` rr
                    in Node l (newND coversParent cp nd) (insertR cp r)
      | otherwise = let cp  = ri `covers` rr
                    in Node (insertR cp l) (newND coversParent cp nd) r

    insert' _            l@(Leaf _) = l
    insert' coversParent (Node l nd@(NodeData m rr _) r)
      | b < m     = Node (insert' False l) nd r
      | m < a     = Node l nd (insertR False r)
      | otherwise = let cp  = ri `covers` rr
                        l'  = if a <= m then insertL cp l else l
                        r'  = if m <= b then insertR cp r else r
                    in Node l' (newND coversParent cp nd) r'




fromIntervals :: NonEmpty (Interval p r) -> SegmentTree [Interval p r] r
fromIntervals = undefined
