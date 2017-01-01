module Algorithms.BinaryTree.BalancedDecomposition where


import           Control.Lens (bimap)
import           Data.BinaryTree
import           Data.BinaryTree.Zipper
import qualified Data.Foldable as F
import           Data.Ord (comparing)
import           Data.Semigroup

--------------------------------------------------------------------------------

-- | Given a binary tree with n nodes, splits it into two trees (s,l) of size
-- $n/3 <= s <= l <= 2n/3$.
--
-- running time: $O(n)$
balancedSplit   :: BinaryTree a -> (BinaryTree a, BinaryTree a)
balancedSplit t = bimap drop' drop' . f
                . splitTree . F.minimumBy (comparing diff') . visitAll $ t''
  where
    -- annotate the tree with subtree sizes
    t'  = foldBinaryUp (Sum 0) (\_ l r -> Sum 1 <> l <> r) t
    -- total size
    n   = maybe 0 (getSum . snd) $ access t'
    -- label with both k and n-k
    t'' = (\(x,Sum k) -> (x,k,n - k)) <$> t'
    -- difference in size between k and n-k
    diff' (Loc tr _) = maybe n (\(_,k,nk) -> abs (k - nk)) . access $ tr

    drop' = fmap (\(x,_,_) -> x) -- unannotate

    -- order the tuple s.t. the largest tree occurs first
    f tt@(a,b) = let g = maybe n (\(_,k,_) -> k) . access
                  in case g a `compare` g b of
                    LT -> tt
                    _  -> (b,a)
