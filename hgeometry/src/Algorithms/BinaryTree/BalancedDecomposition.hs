{-# LANGUAGE TemplateHaskell #-}
module Algorithms.BinaryTree.BalancedDecomposition where


import           Control.Lens
import           Data.BinaryTree
import           Data.BinaryTree.Zipper
import qualified Data.Foldable as F
import           Data.Ord (comparing)
import           Data.Semigroup (Sum(..))
import           Data.Util

--------------------------------------------------------------------------------

-- TODO: return more information, in particular; return which is the original
-- root/what the sizes are etc.

data Split a = Split { _inner :: !(SP (BinaryTree a) Int)
                     , _outer :: !(SP (BinaryTree a) Int)
                     }
               deriving (Show,Eq,Ord)
makeLenses ''Split


-- | Given a binary tree with n nodes, splits it into two trees i,o of size
-- \(n/3 <= i,o <= 2n/3\).
--
-- For n < 3 the inner tree is at least as large as the outer tree
--
-- running time: \(O(n)\)
balancedSplit   :: BinaryTree a -> Split a
balancedSplit t = (\(i,o) -> let i'@(SP _ k) = select i
                             in Split i' (select o &_2 .~ (n - k))
                  )
                . splitTree . F.minimumBy (comparing diff') . visitAll $ t''
  where
    -- annotate the tree with subtree sizes
    t'  = foldBinaryUp (Sum 0) (\_ l r -> Sum 1 <> l <> r) t
    -- total size
    n   = maybe 0 (getSum . snd) $ access t'
    -- label with both k and n-k
    t'' = (\(x,Sum k) -> STR x k (n-k)) <$> t'
    -- difference in size between k and n-k
    diff' (Loc tr _) = maybe n (\(STR _ k nk) -> abs (k - nk)) . access $ tr

    select tx = case access tx of
                  Nothing          -> SP t 0
                  Just (STR _ k _) -> SP (fmap (^._1) tx) k

    -- -- order the tuple s.t. the largest tree occurs first
    -- f tt@(a,b) = let g = maybe n (\(_,k,_) -> k) . access
    --               in case g a `compare` g b of
    --                 LT -> tt
    --                 _  -> (b,a)


-- -- | Computes a balanced decomposition of the input tree, i.e. recursively
-- -- splits into trees of roughly equal size
-- --
-- -- running time: \(O(n\log n)\).
-- balancedDecomposition     :: BinaryTree a -> BinaryTree a
-- balancedDecomposition t = case t of
--     Nil                -> t
--     Internal Nil _ Nil -> t
--     _                  -> go t
--   where
--     go t' = let Split (SP i _) (SP o _) = balancedSplit t'
--             in Internal (balancedDecomposition i)
--                         (fromJust $ access i)
--                         (balancedDecomposition o)
--                         -- if |t| > 0, either i or o is non-zero
--                         -- by construction above this is always i
