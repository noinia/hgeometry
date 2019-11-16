module Algorithms.DivideAndConquer( divideAndConquer
                                  , divideAndConquer1
                                  , divideAndConquer1With
                                  ) where

import           Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NonEmpty


-- | Divide and conquer strategy
--
-- the running time is: O(n*L) + T(n) = 2T(n/2) + M(n)
--
-- where M(n) is the time corresponding to the semigroup operation of s
--
divideAndConquer1 :: Semigroup s => (a -> s) -> NonEmpty a -> s
divideAndConquer1 = divideAndConquer1With (<>)


-- | Divide and conquer for
divideAndConquer   :: Monoid s => (a -> s) -> [a] -> s
divideAndConquer g = maybe mempty (divideAndConquer1 g) . NonEmpty.nonEmpty

-- | Divide and conquer strategy
--
-- the running time is: O(n*L) + T(n) = 2T(n/2) + M(n)
--
-- where M(n) is the time corresponding to the merge operation s
--
divideAndConquer1With         :: (s -> s -> s) -> (a -> s) -> NonEmpty a -> s
divideAndConquer1With (<.>) g = repeatedly merge . fmap g
  where
    repeatedly _ (t :| []) = t
    repeatedly f ts        = repeatedly f $ f ts

    merge ts@(_ :| [])  = ts
    merge (l :| r : []) = l <.> r :| []
    merge (l :| r : ts) = l <.> r <| (merge $ NonEmpty.fromList ts)
