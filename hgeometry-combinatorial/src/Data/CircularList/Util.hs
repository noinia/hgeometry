module Data.CircularList.Util where

import           Control.Lens
import           Data.Tuple
import qualified Data.CircularList as C
import qualified Data.List as L


--------------------------------------------------------------------------------

-- $setup
-- >>> let ordList = C.fromList [5,6,10,20,30,1,2,3]



-- | Given a circular list, whose elements are in increasing order, insert the
-- new element into the Circular list in its sorted order.
--
-- >>> insertOrd 1 C.empty
-- fromList [1]
-- >>> insertOrd 1 $ C.fromList [2]
-- fromList [2,1]
-- >>> insertOrd 2 $ C.fromList [1,3]
-- fromList [1,2,3]
-- >>> insertOrd 31 ordList
-- fromList [5,6,10,20,30,31,1,2,3]
-- >>> insertOrd 1 ordList
-- fromList [5,6,10,20,30,1,1,2,3]
-- >>> insertOrd 4 ordList
-- fromList [5,6,10,20,30,1,2,3,4]
-- >>> insertOrd 11 ordList
-- fromList [5,6,10,11,20,30,1,2,3]
insertOrd :: Ord a => a -> C.CList a -> C.CList a
insertOrd = insertOrdBy compare

-- | Insert an element into an increasingly ordered circular list, with
-- specified compare operator.
insertOrdBy       :: (a -> a -> Ordering) -> a -> C.CList a -> C.CList a
insertOrdBy cmp x = C.fromList . insertOrdBy' cmp x . C.rightElements

-- | List version of insertOrdBy; i.e. the list contains the elements in
-- cirulcar order. Again produces a list that has the items in circular order.
insertOrdBy'         :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertOrdBy' cmp x xs = case (rest, x `cmp` head rest) of
    ([],  _)   -> L.insertBy cmp x pref
    (z:zs, GT) -> (z : L.insertBy cmp x zs) ++ pref
    (_:_,  EQ) -> (x : xs) -- == x : rest ++ pref
    (_:_,  LT) -> rest ++ L.insertBy cmp x pref
  where
    -- split the list at its maximum.
    (pref,rest) = splitIncr cmp xs

-- given a list of elements that is supposedly a a cyclic-shift of a list of
-- increasing items, find the splitting point. I.e. returns a pair of lists
-- (ys,zs) such that xs = zs ++ ys, and ys ++ zs is (supposedly) in sorted
-- order.
splitIncr              :: (a -> a -> Ordering) -> [a] -> ([a],[a])
splitIncr _   []       = ([],[])
splitIncr cmp xs@(x:_) = swap . bimap (map snd) (map snd)
                      . L.break (\(a,b) -> (a `cmp` b) == GT) $ zip (x:xs) xs

-- | Test if the circular list is a cyclic shift of the second list.
-- Running time: O(n), where n is the size of the smallest list
isShiftOf         :: Eq a => C.CList a -> C.CList a -> Bool
xs `isShiftOf` ys = let rest = tail . C.leftElements
                    in maybe False (\xs' -> rest xs' == rest ys) $
                         C.focus ys >>= flip C.rotateTo xs
