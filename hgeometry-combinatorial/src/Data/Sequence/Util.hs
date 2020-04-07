module Data.Sequence.Util where

import           Algorithms.BinarySearch
import qualified Data.Sequence as S
import           Data.Sequence (Seq)

--------------------------------------------------------------------------------

-- | Partition the seq s given a monotone predicate p into (xs,ys) such that
--
-- all elements in xs do *not* satisfy the predicate p
-- all elements in ys do       satisfy the predicate p
--
-- all elements in s occur in either xs or ys.
--
-- running time: \(O(\log^2 n + T*\log n)\), where \(T\) is the time to execute the
-- predicate.
splitMonotone     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
splitMonotone p s = case binarySearchSeq p s of
                      Nothing -> (s,S.empty)
                      Just i  -> S.splitAt i s
