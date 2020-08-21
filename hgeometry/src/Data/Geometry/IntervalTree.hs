{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.IntervalTree( NodeData(..)
                                 , splitPoint, intervalsLeft, intervalsRight
                                 , IntervalTree(..), unIntervalTree
                                 , IntervalLike(..)
                                 , createTree, fromIntervals
                                 , insert, delete
                                 , stab, search
                                 , toList
                                 ) where


import           Control.DeepSeq
import           Control.Lens
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.Interval.Util
import           Data.Geometry.Properties
import qualified Data.List as List
import qualified Data.Map as M
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Information stored in a node of the Interval Tree
data NodeData i r = NodeData { _splitPoint     :: !r
                             , _intervalsLeft  :: !(M.Map (L r) [i])
                             , _intervalsRight :: !(M.Map (R r) [i])
                             } deriving (Show,Eq,Ord,Generic)
makeLenses ''NodeData

instance (NFData i, NFData r) => NFData (NodeData i r)

-- | IntervalTree type, storing intervals of type i
newtype IntervalTree i r =
  IntervalTree { _unIntervalTree :: BinaryTree (NodeData i r) }
  deriving (Show,Eq,Generic)
makeLenses ''IntervalTree

instance (NFData i, NFData r) => NFData (IntervalTree i r)

-- | Given an ordered list of points, create an interval tree
--
-- \(O(n)\)
createTree     :: Ord r => [r] -> IntervalTree i r
createTree pts = IntervalTree . asBalancedBinTree
               . map (\m -> NodeData m mempty mempty) $ pts


-- | Build an interval tree
--
-- \(O(n \log n)\)
fromIntervals    :: (Ord r, IntervalLike i, NumType i ~ r)
                 => [i] -> IntervalTree i r
fromIntervals is = foldr insert (createTree pts) is
  where
    endPoints (asRange -> Range' a b) = [a,b]
    endPoints _ = error "Unreachable, but cannot prove it in Haskell"
    pts = List.sort . concatMap endPoints $ is

-- | Lists the intervals. We don't guarantee anything about the order
--
-- running time: \(O(n)\).
toList :: IntervalTree i r -> [i]
toList = toList' . _unIntervalTree
  where
    toList' Nil              = []
    toList' (Internal l v r) =
      concat [concat $ v^..intervalsLeft.traverse, toList' l, toList' r]

--------------------------------------------------------------------------------

-- | Find all intervals that stab x
--
-- \(O(\log n + k)\), where k is the output size
search :: Ord r => r -> IntervalTree i r -> [i]
search = stab

-- | Find all intervals that stab x
--
-- \(O(\log n + k)\), where k is the output size
stab                    :: Ord r => r -> IntervalTree i r -> [i]
stab x (IntervalTree t) = stab' t
  where
    stab' Nil = []
    stab' (Internal l (NodeData m ll rr) r)
      | x <= m    = let is = f (<= L (Closed x)) . M.toAscList $ ll
                    in is ++ stab' l
      | otherwise = let is = f (>= R (Closed x)) . M.toDescList $ rr
                    in is ++ stab' r
    f p = concatMap snd . List.takeWhile (p . fst)

--------------------------------------------------------------------------------

-- | Insert :
-- pre: the interval intersects some midpoint in the tree
--
-- \(O(\log n)\)
insert                    :: (Ord r, IntervalLike i, NumType i ~ r)
                          => i -> IntervalTree i r -> IntervalTree i r
insert i (IntervalTree t) = IntervalTree $ insert' t
  where
    ri@(Range a b) = asRange i

    insert' Nil = Nil
    insert' (Internal l nd@(_splitPoint -> m) r)
      | m `inRange` ri = Internal l (insert'' nd) r
      | b <= Closed m  = Internal (insert' l) nd r
      | otherwise      = Internal l nd (insert' r)

    insert'' (NodeData m l r) = NodeData m (M.insertWith (++) (L a) [i] l)
                                           (M.insertWith (++) (R b) [i] r)


-- | Delete an interval from the Tree
--
-- \(O(\log n)\) (under some general position assumption)
delete :: (Ord r, IntervalLike i, NumType i ~ r, Eq i)
          => i -> IntervalTree i r -> IntervalTree i r
delete i (IntervalTree t) = IntervalTree $ delete' t
  where
    ri@(Range a b) = asRange i

    delete' Nil = Nil
    delete' (Internal l nd@(_splitPoint -> m) r)
      | m `inRange` ri = Internal l (delete'' nd) r
      | b <= Closed m  = Internal (delete' l) nd r
      | otherwise      = Internal l nd (delete' r)

    delete'' (NodeData m l r) = NodeData m (M.update f (L a) l) (M.update f (R b) r)
    f is = let is' = List.delete i is in if null is' then Nothing else Just is'



--------------------------------------------------------------------------------


-- | Anything that looks like an interval
class IntervalLike i where
  asRange :: i -> Range (NumType i)

instance IntervalLike (Range r) where
  asRange = id

instance IntervalLike (Interval p r) where
  asRange = fmap (^.core) . toRange

--------------------------------------------------------------------------------

-- test'' = fromIntervals test
-- test  = [Interval (Open (97 :+ ())) (Closed (228 :+ ())) ,Interval (Open (18 :+ ())) (Open (79 :+ ())),Interval (Closed (126 :+ ())) (Open (167 :+ ())),Interval (Closed (105 :+ ())) (Closed (158 :+ ())),Interval (Closed (126 :+ ())) (Closed (211 :+ ())),Interval (Closed (111 :+ ())) (Open (194 :+ ())),Interval (Closed (120 :+ ())) (Open (302 :+ ())),Interval (Closed (92 :+ ())) (Closed (140 :+ ()))]

-- test = fromIntervals [ closedInterval 0 10
--                      , closedInterval 5 15
--                      , closedInterval 1 4
--                      , closedInterval 3 9
--                      ]

-- closedInterval a b = ClosedInterval (ext a) (ext b)
