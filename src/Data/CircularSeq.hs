module Data.CircularSeq where

import           Control.Applicative
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe)
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Sequence as S
import           Data.Sequence ((|>),(<|),ViewL(..),ViewR(..),Seq)
import qualified Data.Traversable as T
import           Data.Tuple (swap)

--------------------------------------------------------------------------------

-- | Nonempty circular sequence
data CSeq a = CSeq (Seq a) a (Seq a)
            deriving (Show,Read,Eq)
                     -- we keep the seq balanced, i.e. size left >= size right


-- traverses starting at the focus, going to the right.
instance T.Traversable CSeq where
  traverse f (CSeq l x r) = (\x' r' l' -> CSeq l' x' r')
                         <$> f x <*> traverse f r <*> traverse f l

instance Foldable CSeq where
  foldMap = T.foldMapDefault
  length (CSeq l _ r) = 1 + S.length l + S.length r


instance Functor CSeq where
  fmap = T.fmapDefault

focus              :: CSeq a -> a
focus (CSeq _ x _) = x


resplit   :: Seq a -> (Seq a, Seq a)
resplit s = swap $ S.splitAt (length s `div` 2) s


withFocus     :: a -> Seq a -> CSeq a
withFocus x s = let (l,r) = resplit s in CSeq l x r


rotateR                :: CSeq a -> CSeq a
rotateR s@(CSeq l x r) = case S.viewl (r <> l) of
                           EmptyL   -> s
                           (y :< t) -> withFocus y (t |> x)


rotateL                :: CSeq a -> CSeq a
rotateL s@(CSeq l x r) = case S.viewr (r <> l) of
                           EmptyR   -> s
                           (t :> y) -> withFocus y (t |> x)


asSeq              :: CSeq a -> Seq a
asSeq (CSeq l x r) = (l |> x) <> r


-- >>> rightElements $ fromList [3,4,5,1,2]
-- fromList [3,4,5,1,2]
rightElements              :: CSeq a -> Seq a
rightElements (CSeq l x r) = x <| r <> l


-- >>> leftElements $ fromList [3,4,5,1,2]
-- fromList [3,2,1,5,4]
leftElements              :: CSeq a -> Seq a
leftElements (CSeq l x r) = x <| S.reverse l <> S.reverse r


fromNonEmpty                    :: NonEmpty.NonEmpty a -> CSeq a
fromNonEmpty (x NonEmpty.:| xs) = withFocus x $ S.fromList xs

fromList        :: [a] -> CSeq a
fromList (x:xs) = withFocus x $ S.fromList xs
fromList []     = error "fromList: Empty list"

-- | Rotates i elements to the right.
-- pre: 0 <= i < n
rotateNR     :: Int -> CSeq a -> CSeq a
rotateNR i s = let (l, r')  = S.splitAt i $ rightElements s
                   (x :< r) = S.viewl r'
               in withFocus x $ r <> l

-- pre: 0 <= i < n
-- rotateNL i s = let (r',l)   = S.splitAt (length s - i) $ rightElements s
--                    (r :> x) = S.viewr r'
--                in withFocus x $ l <> r


-- >>> reverseDirection $ fromList [1..5]
-- CSeq (fromList [3,2]) 1 (fromList [5,4])
reverseDirection              :: CSeq a -> CSeq a
reverseDirection (CSeq l x r) = CSeq (S.reverse r) x (S.reverse l)


-- >>> findRotateTo (== 3) $ fromList [1..5]
-- Just (CSeq (fromList [1,2]) 3 (fromList [4,5]))
-- >>> findRotateTo (== 7) $ fromList [1..5]
-- Nothing
findRotateTo   :: (a -> Bool) -> CSeq a -> Maybe (CSeq a)
findRotateTo p = listToMaybe . filter (p . focus) . allRotations'

-- >>> mapM_ print . allRotations $ fromList [1..5]
-- CSeq (fromList [4,5]) 1 (fromList [2,3])
-- CSeq (fromList [5,1]) 2 (fromList [3,4])
-- CSeq (fromList [1,2]) 3 (fromList [4,5])
-- CSeq (fromList [2,3]) 4 (fromList [5,1])
-- CSeq (fromList [3,4]) 5 (fromList [1,2])
allRotations :: CSeq a -> CSeq (CSeq a)
allRotations = fromList . allRotations'

allRotations'   :: CSeq a -> [CSeq a]
allRotations' s = take (length s) . iterate rotateR $ s
