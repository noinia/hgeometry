module Data.CircularSeq( CSeq
                       , cseq
                       , singleton
                       , fromNonEmpty
                       , fromList

                       , focus
                       , index, adjust
                       , item

                       , rotateL
                       , rotateR
                       , rotateNL, rotateNR

                       , rightElements
                       , leftElements
                       , asSeq

                       , reverseDirection
                       , allRotations

                       , findRotateTo
                       , rotateTo

                       , zipLWith, zipL
                       , zip3LWith


                       , insertOrd, insertOrdBy
                       , isShiftOf
                       ) where

import           Algorithms.StringSearch.KMP (isSubStringOf)
import           Control.DeepSeq
import           Control.Lens (lens, Lens', bimap)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe, isJust)
import           Data.Semigroup.Foldable
import           Data.Sequence ((|>),(<|),ViewL(..),ViewR(..),Seq)
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import           Data.Tuple (swap)
import           GHC.Generics (Generic)
import           Test.QuickCheck(Arbitrary(..))
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

-- $setup
-- >>> let ordList = fromList [5,6,10,20,30,1,2,3]


-- | Nonempty circular sequence
data CSeq a = CSeq !(Seq a) !a !(Seq a)
  deriving (Generic)
                     -- we keep the seq balanced, i.e. size left >= size right

instance NFData a => NFData (CSeq a)

instance Eq a => Eq (CSeq a) where
  a == b = asSeq a == asSeq b

instance Show a => Show (CSeq a) where
  showsPrec d s = showParen (d > app_prec) $
                    showString (("CSeq " <>) . show . F.toList . rightElements $ s)
    where app_prec = 10

-- traverses starting at the focus, going to the right.
instance T.Traversable CSeq where
  traverse f (CSeq l x r) = (\x' r' l' -> CSeq l' x' r')
                         <$> f x <*> traverse f r <*> traverse f l
-- instance Traversable1 CSeq where
--   traverse1 f (CSeq l x r) = liftF3 (\x' r' l' -> CSeq l' x' r')
--                                     (f x) (traverse f r) (traverse f l)

instance Foldable1 CSeq

instance F.Foldable CSeq where
  foldMap = T.foldMapDefault
  length (CSeq l _ r) = 1 + S.length l + S.length r

instance Functor CSeq where
  fmap = T.fmapDefault

instance Arbitrary a => Arbitrary (CSeq a) where
  arbitrary = CSeq <$> arbitrary <*> arbitrary <*> arbitrary

singleton   :: a -> CSeq a
singleton x = CSeq S.empty x S.empty

-- | Gets the focus of the CSeq
-- running time: O(1)
focus              :: CSeq a -> a
focus (CSeq _ x _) = x

-- | Access the i^th item  (w.r.t the focus) in the CSeq (indices modulo n).
--
-- running time: \(O(\log (i \mod n))\)
--
-- >>> index (fromList [0..5]) 1
-- 1
-- >>> index (fromList [0..5]) 2
-- 2
-- >>> index (fromList [0..5]) 5
-- 5
-- >>> index (fromList [0..5]) 10
-- 4
-- >>> index (fromList [0..5]) 6
-- 0
-- >>> index (fromList [0..5]) (-1)
-- 5
-- >>> index (fromList [0..5]) (-6)
-- 0
index                   :: CSeq a -> Int -> a
index s@(CSeq l x r) i' = let i  = i' `mod` length s
                              rn = length r
                          in if i == 0 then x
                               else if i - 1 < rn then S.index r (i - 1)
                                                  else S.index l (i - rn - 1)

-- | Adjusts the i^th element w.r.t the focus in the CSeq
--
-- running time: \(O(\log (i \mod n))\)
--
-- >>> adjust (const 1000) 2 (fromList [0..5])
-- CSeq [0,1,1000,3,4,5]
adjust                     :: (a -> a) -> Int -> CSeq a -> CSeq a
adjust f i' s@(CSeq l x r) = let i  = i' `mod` length s
                                 rn = length r
                             in if i == 0 then CSeq l (f x) r
                                else if i - 1 < rn
                                     then CSeq l                           x (S.adjust f (i - 1) r)
                                     else CSeq (S.adjust f (i - rn - 1) l) x r


-- | Access te ith item in the CSeq (w.r.t the focus) as a lens
item   :: Int -> Lens' (CSeq a) a
item i = lens (flip index i) (\s x -> adjust (const x) i s)


resplit   :: Seq a -> (Seq a, Seq a)
resplit s = swap $ S.splitAt (length s `div` 2) s


-- | smart constructor that automatically balances the seq
cseq                   :: Seq a -> a -> Seq a -> CSeq a
cseq l x r
    | ln > 1 + 2*rn    = withFocus x (r <> l)
    | ln < rn `div`  2 = withFocus x (r <> l)
    | otherwise        = CSeq l x r
  where
    rn = length r
    ln = length l

-- smart constructor that automatically balances the sequence.
-- pre: at least one of the two seq's is NonEmpty
--
cseq'     :: Seq a -> Seq a -> CSeq a
cseq' l r = case S.viewl r of
              (x :< r') -> cseq l x r'
              EmptyL    -> let (x :< l') = S.viewl l in cseq l' x r

-- | Builds a balanced seq with the element as the focus.
withFocus     :: a -> Seq a -> CSeq a
withFocus x s = let (l,r) = resplit s in CSeq l x r

-- | rotates one to the right
--
-- running time: O(1) (amortized)
--
-- >>> rotateR $ fromList [3,4,5,1,2]
-- CSeq [4,5,1,2,3]
rotateR                :: CSeq a -> CSeq a
rotateR s@(CSeq l x r) = case S.viewl r of
                           EmptyL    -> case S.viewl l of
                             EmptyL    -> s
                             (y :< l') -> cseq (S.singleton x) y l'
                           (y :< r') -> cseq (l |> x) y r'

-- | rotates the focus to the left
--
-- running time: O(1) (amortized)
--
-- >>> rotateL $ fromList [3,4,5,1,2]
-- CSeq [2,3,4,5,1]
-- >>> mapM_ print . take 5 $ iterate rotateL $ fromList [1..5]
-- CSeq [1,2,3,4,5]
-- CSeq [5,1,2,3,4]
-- CSeq [4,5,1,2,3]
-- CSeq [3,4,5,1,2]
-- CSeq [2,3,4,5,1]
rotateL                :: CSeq a -> CSeq a
rotateL s@(CSeq l x r) = case S.viewr l of
                           EmptyR    -> case S.viewr r of
                             EmptyR     -> s
                             (r' :> y)  -> cseq r' y (S.singleton x)
                           (l' :> y) -> cseq l' y (x <| r)


-- | Convert to a single Seq, starting with the focus.
asSeq :: CSeq a -> Seq a
asSeq = rightElements


-- | All elements, starting with the focus, going to the right

-- >>> rightElements $ fromList [3,4,5,1,2]
-- fromList [3,4,5,1,2]
rightElements              :: CSeq a -> Seq a
rightElements (CSeq l x r) = x <| r <> l


-- | All elements, starting with the focus, going to the left
--
-- >>> leftElements $ fromList [3,4,5,1,2]
-- fromList [3,2,1,5,4]
leftElements              :: CSeq a -> Seq a
leftElements (CSeq l x r) = x <| S.reverse l <> S.reverse r

-- | builds a CSeq
fromNonEmpty                    :: NonEmpty.NonEmpty a -> CSeq a
fromNonEmpty (x NonEmpty.:| xs) = withFocus x $ S.fromList xs

fromList        :: [a] -> CSeq a
fromList (x:xs) = withFocus x $ S.fromList xs
fromList []     = error "fromList: Empty list"

-- | Rotates i elements to the right.
--
-- pre: 0 <= i < n
--
-- running time: \(O(\log i)\) amortized
--
-- >>> rotateNR 0 $ fromList [1..5]
-- CSeq [1,2,3,4,5]
-- >>> rotateNR 1 $ fromList [1..5]
-- CSeq [2,3,4,5,1]
-- >>> rotateNR 4 $ fromList [1..5]
-- CSeq [5,1,2,3,4]
rotateNR   :: Int -> CSeq a -> CSeq a
rotateNR i = uncurry cseq' . S.splitAt i . rightElements

-- | Rotates i elements to the left.
--
-- pre: 0 <= i < n
--
-- running time: \(O(\log i)\) amoritzed
--
-- >>> rotateNL 0 $ fromList [1..5]
-- CSeq [1,2,3,4,5]
-- >>> rotateNL 1 $ fromList [1..5]
-- CSeq [5,1,2,3,4]
-- >>> rotateNL 2 $ fromList [1..5]
-- CSeq [4,5,1,2,3]
-- >>> rotateNL 3 $ fromList [1..5]
-- CSeq [3,4,5,1,2]
-- >>> rotateNL 4 $ fromList [1..5]
-- CSeq [2,3,4,5,1]
rotateNL     :: Int -> CSeq a -> CSeq a
rotateNL i s = let (x :< xs) = S.viewl $ rightElements s
                   (l',r)    = S.splitAt (length s - i) $ xs |> x
               in case S.viewr l' of
                    l :> y   -> cseq l y r
                    S.EmptyR -> let (y :< r') = S.viewl r in cseq l' y r'


-- | Reversres the direction of the CSeq
--
-- running time: \(O(n)\)
--
-- >>> reverseDirection $ fromList [1..5]
-- CSeq [1,5,4,3,2]
reverseDirection              :: CSeq a -> CSeq a
reverseDirection (CSeq l x r) = CSeq (S.reverse r) x (S.reverse l)


-- | Finds an element in the CSeq
--
-- >>> findRotateTo (== 3) $ fromList [1..5]
-- Just (CSeq [3,4,5,1,2])
-- >>> findRotateTo (== 7) $ fromList [1..5]
-- Nothing
findRotateTo   :: (a -> Bool) -> CSeq a -> Maybe (CSeq a)
findRotateTo p = listToMaybe . filter (p . focus) . allRotations'


rotateTo   :: Eq a => a -> CSeq a -> Maybe (CSeq a)
rotateTo x = findRotateTo (== x)


-- | All rotations, the input CSeq is the focus.
--
-- >>> mapM_ print . allRotations $ fromList [1..5]
-- CSeq [1,2,3,4,5]
-- CSeq [2,3,4,5,1]
-- CSeq [3,4,5,1,2]
-- CSeq [4,5,1,2,3]
-- CSeq [5,1,2,3,4]
allRotations :: CSeq a -> CSeq (CSeq a)
allRotations = fromList . allRotations'

allRotations'   :: CSeq a -> [CSeq a]
allRotations' s = take (length s) . iterate rotateR $ s

-- | "Left zip": zip the two CLists, pairing up every element in the *left*
-- list with its corresponding element in the right list. If there are more
-- items in the right clist they are discarded.
zipLWith         :: (a -> b -> c) -> CSeq a -> CSeq b -> CSeq c
zipLWith f as bs = fromList $ zipWith f (F.toList as) (F.toList bs)

-- | see 'zipLWith
zipL :: CSeq a -> CSeq b -> CSeq (a, b)
zipL = zipLWith (,)


-- | same as zipLWith but with three items
zip3LWith            :: (a -> b -> c -> d) -> CSeq a -> CSeq b -> CSeq c -> CSeq d
zip3LWith f as bs cs = fromList $ zipWith3 f (F.toList as) (F.toList bs) (F.toList cs)




-- | Given a circular seq, whose elements are in increasing order, insert the
-- new element into the Circular seq in its sorted order.
--
-- >>> insertOrd 1 $ fromList [2]
-- CSeq [2,1]
-- >>> insertOrd 2 $ fromList [1,3]
-- CSeq [1,2,3]
-- >>> insertOrd 31 ordList
-- CSeq [5,6,10,20,30,31,1,2,3]
-- >>> insertOrd 1 ordList
-- CSeq [5,6,10,20,30,1,1,2,3]
-- >>> insertOrd 4 ordList
-- CSeq [5,6,10,20,30,1,2,3,4]
-- >>> insertOrd 11 ordList
-- CSeq [5,6,10,11,20,30,1,2,3]
--
-- running time: \(O(n)\)
insertOrd :: Ord a => a -> CSeq a -> CSeq a
insertOrd = insertOrdBy compare

-- | Insert an element into an increasingly ordered circular list, with
-- specified compare operator.
--
-- running time: \(O(n)\)
insertOrdBy       :: (a -> a -> Ordering) -> a -> CSeq a -> CSeq a
insertOrdBy cmp x = fromList . insertOrdBy' cmp x . F.toList . rightElements

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

-- | Test if the circular list is a cyclic shift of the second
-- list. We have that
--
-- prop> (xs `isShiftOf` ys) == (xs `elem` allRotations (ys :: CSeq Int))
--
-- Running time: \(O(n+m)\), where \(n\) and \(m\) are the sizes of
-- the lists.
isShiftOf         :: Eq a => CSeq a -> CSeq a -> Bool
xs `isShiftOf` ys = let twice zs    = let zs' = leftElements zs in zs' <> zs'
                        once        = leftElements
                        check as bs = isJust $ once as `isSubStringOf` twice bs
                    in length xs == length ys && check xs ys
