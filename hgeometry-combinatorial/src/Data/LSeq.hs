{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.LSeq
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Wrapper around Data.Sequence with type level length annotation.
--
--------------------------------------------------------------------------------
module Data.LSeq( LSeq( EmptyL, (:<|), (:<<), (:|>) )
                , toSeq
                , empty
                , fromList
                , fromNonEmpty
                , fromSeq

                , (<|), (|>)
                , (><)
                , eval

                , index
                , adjust
                , partition
                , mapWithIndex
                , take
                , drop
                , unstableSort, unstableSortBy
                , head, tail, last, init
                , append

                , ViewL(..)
                , viewl

                , ViewR(..)
                , viewr

                , zipWith

                , promise
                , forceLSeq
                ) where

import           Control.DeepSeq
import           Control.Lens ((%~), (&), (<&>), (^?!), bimap)
import           Control.Lens.At (Ixed(..), Index, IxValue)
import           Data.Aeson
import           Data.Coerce(coerce)
import qualified Data.Foldable as F
import           Data.Functor.Apply
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Prelude hiding (drop,take,head,last,tail,init,zipWith)
import           Test.QuickCheck (Arbitrary(..),vector)

--------------------------------------------------------------------------------

-- $setup
-- >>> :{
-- import Data.Proxy
-- :}



-- | LSeq n a certifies that the sequence has *at least* n items
newtype LSeq (n :: Nat) a = LSeq (S.Seq a)
                          deriving (Show,Read,Eq,Ord,Foldable,Functor,Traversable
                                   ,Generic,NFData)

-- | \( O(1) \) Convert to a sequence by dropping the type-level size.
toSeq          :: LSeq n a -> S.Seq a
toSeq (LSeq s) = s

instance Semigroup (LSeq n a) where
  (LSeq s) <> (LSeq s') = LSeq (s <> s')

instance Monoid (LSeq 0 a) where
  mempty = empty
  mappend = (<>)

instance (KnownNat n, Arbitrary a) => Arbitrary (LSeq n a) where
  arbitrary = (\s s' -> promise . fromList $ s <> s')
            <$> vector (fromInteger . natVal $ (Proxy :: Proxy n))
            <*> arbitrary

instance ToJSON a => ToJSON (LSeq n a) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON a => FromJSON (LSeq n a)


type instance Index   (LSeq n a) = Int
type instance IxValue (LSeq n a) = a
instance Ixed (LSeq n a) where
  ix i f s@(LSeq xs)
    | 0 <= i && i < S.length xs = f (S.index xs i) <&> \x -> LSeq $ S.update i x xs
    | otherwise                 = pure s

instance (1 <= n) => Foldable1 (LSeq n)
-- instance (1 <= n) => Traversable1 (LSeq n) where
--   traverse1 f s = case traverse1 f $ viewl s of
--                     x :< s' -> x <| s'

-- | \( O(1) \) The empty sequence.
empty :: LSeq 0 a
empty = LSeq S.empty

-- | \( O(1) \) Add an element to the left end of a sequence.
--   Mnemonic: a triangle with the single element at the pointy end.
(<|) :: a -> LSeq n a -> LSeq (1 + n) a
x <| xs = LSeq (x S.<| toSeq xs)

-- | \( O(1) \) Add an element to the right end of a sequence.
--   Mnemonic: a triangle with the single element at the pointy end.
(|>)    :: LSeq n a -> a -> LSeq (1 + n) a
xs |> x = LSeq (toSeq xs S.|> x)

infixr 5 <|
infixl 5 |>

-- | \( O(log(min(n,m))) \) Concatenate two sequences.
(><) :: LSeq n a -> LSeq m a -> LSeq (n + m) a
xs >< ys = LSeq (toSeq xs <> toSeq ys)

infix 5 ><

-- | \( O(1) \) Prove a sequence has at least @n@ elements.
--
-- >>> eval (Proxy :: Proxy 3) (fromList [1,2,3])
-- Just (LSeq (fromList [1,2,3]))
-- >>> eval (Proxy :: Proxy 3) (fromList [1,2])
-- Nothing
-- >>> eval (Proxy :: Proxy 3) (fromList [1..10])
-- Just (LSeq (fromList [1,2,3,4,5,6,7,8,9,10]))
eval :: forall proxy n m a. KnownNat n => proxy n -> LSeq m a -> Maybe (LSeq n a)
eval n (LSeq xs)
  | toInteger (S.length xs) >= natVal n = Just $ LSeq xs
  | otherwise                           = Nothing





-- | Promises that the length of this LSeq is actually n. This is not
-- checked.
--
-- This function should be a noop
promise :: forall m n a. LSeq m a -> LSeq n a
promise = coerce


-- | Forces the first n elements of the LSeq
forceLSeq   :: KnownNat n => proxy n -> LSeq m a -> LSeq n a
forceLSeq n = promise . go (fromInteger $ natVal n)
  where
    -- forces the Lseq for n' positions
    go                    :: Int -> LSeq m a -> LSeq m a
    go !n' s | n' <= l    = s
             | otherwise  = error msg
      where
        !l  = S.length . S.take n' . toSeq $ s
        msg = "forceLSeq: too few elements. expected " <> show n' <> " but found " <> show l


-- | appends two sequences.
--
append         :: LSeq n a -> LSeq m a -> LSeq (n + m) a
sa `append` sb = LSeq $ toSeq sa <> toSeq sb

--------------------------------------------------------------------------------

-- | \( O(log(min(i,n-i))) \)
--   Get the element with index i, counting from the left and starting at 0.
index     :: LSeq n a -> Int -> a
index s i = s^?!ix i

-- | \( O(log(min(i,n−i))) \) Update the element at the specified position. If the
--   position is out of range, the original sequence is returned. adjust can lead
--   to poor performance and even memory leaks, because it does not force the new
--   value before installing it in the sequence. adjust' should usually be preferred.
adjust       :: (a -> a) -> Int -> LSeq n a -> LSeq n a
adjust f i s = s&ix i %~ f

-- | \( O(n) \) The partition function takes a predicate p and a sequence xs and
--   returns sequences of those elements which do and do not satisfy the predicate.
partition   :: (a -> Bool) -> LSeq n a -> (LSeq 0 a, LSeq 0 a)
partition p = bimap LSeq LSeq . S.partition p . toSeq

-- | A generalization of 'fmap', 'mapWithIndex' takes a mapping
-- function that also depends on the element's index, and applies it to every
-- element in the sequence.
mapWithIndex   :: (Int -> a -> b) -> LSeq n a -> LSeq n b
mapWithIndex f = wrapUnsafe (S.mapWithIndex f)

-- | \( O(\log(\min(i,n-i))) \). The first @i@ elements of a sequence.
-- If @i@ is negative, @'take' i s@ yields the empty sequence.
-- If the sequence contains fewer than @i@ elements, the whole sequence
-- is returned.
take   :: Int -> LSeq n a -> LSeq 0 a
take i = wrapUnsafe (S.take i)

-- | \( O(\log(\min(i,n-i))) \). Elements of a sequence after the first @i@.
-- If @i@ is negative, @'drop' i s@ yields the whole sequence.
-- If the sequence contains fewer than @i@ elements, the empty sequence
-- is returned.
drop   :: Int -> LSeq n a -> LSeq 0 a
drop i = wrapUnsafe (S.drop i)

-- | \( O(n \log n) \).  A generalization of 'unstableSort', 'unstableSortBy'
-- takes an arbitrary comparator and sorts the specified sequence.
-- The sort is not stable.  This algorithm is frequently faster and
-- uses less memory than 'sortBy'.
unstableSortBy   :: (a -> a -> Ordering) -> LSeq n a -> LSeq n a
unstableSortBy f = wrapUnsafe (S.unstableSortBy f)

-- | \( O(n \log n) \).  'unstableSort' sorts the specified 'Seq' by
-- the natural ordering of its elements, but the sort is not stable.
-- This algorithm is frequently faster and uses less memory than 'sort'.
unstableSort :: Ord a => LSeq n a -> LSeq n a
unstableSort = wrapUnsafe S.unstableSort


wrapUnsafe :: (S.Seq a -> S.Seq b) -> LSeq n a -> LSeq m b
wrapUnsafe f = LSeq . f . toSeq

--------------------------------------------------------------------------------

-- | \( O(n) \). Create an l-sequence from a sequence of elements.
fromSeq :: S.Seq a -> LSeq 0 a
fromSeq = LSeq

-- | \( O(n) \). Create an l-sequence from a finite list of elements.
fromList :: Foldable f => f a -> LSeq 0 a
fromList = LSeq . S.fromList . F.toList

-- | \( O(n) \). Create an l-sequence from a non-empty list.
fromNonEmpty :: NonEmpty.NonEmpty a -> LSeq 1 a
fromNonEmpty = LSeq . S.fromList . F.toList


--------------------------------------------------------------------------------

-- | View of the left end of a sequence.
data ViewL n a where
  (:<) :: a -> LSeq n a -> ViewL (1 + n) a

infixr 5 :<

instance Semigroup (ViewL n a) where
  (x :< xs) <> (y :< ys) = x :< LSeq (toSeq xs <> (y S.<| toSeq ys))

deriving instance Show a => Show (ViewL n a)
instance Functor (ViewL n) where
  fmap = Tr.fmapDefault
instance Foldable (ViewL n) where
  foldMap = Tr.foldMapDefault
instance Traversable (ViewL n) where
  traverse f (x :< xs) = (:<) <$> f x <*> traverse f xs
instance (1 <= n) => Foldable1 (ViewL n)
instance (1 <= n) => Traversable1 (ViewL n) where
  traverse1 f (a :< LSeq as) = (\(b :< bs) -> b :< promise bs) <$> go a as
    where
      go x = \case
        S.Empty       -> (:< empty) <$> f x
        (y S.:<| ys) -> (\x' (y' :< ys') -> x' :< promise @1 @0 (y' :<| ys'))
                        <$> f x <.> go y ys

instance Eq a => Eq (ViewL n a) where
  s == s' = F.toList s == F.toList s'
instance Ord a => Ord (ViewL n a) where
  s `compare` s' = F.toList s `compare` F.toList s'

-- | \( O(1) )\. Analyse the left end of a sequence.
viewl :: LSeq (1 + n) a -> ViewL (1 + n) a
viewl xs = let ~(x S.:< ys) = S.viewl $ toSeq xs in x :< LSeq ys

viewl'    :: LSeq (1 + n) a -> (a, LSeq n a)
viewl' xs = let ~(x S.:< ys) = S.viewl $ toSeq xs in (x,LSeq ys)

infixr 5 :<|

-- | A bidirectional pattern synonym viewing the front of a non-empty
-- sequence.
pattern (:<|)    :: a -> LSeq n a -> LSeq (1 + n) a
pattern x :<| xs <- (viewl' -> (x,xs)) -- we need the coerce unfortunately
  where
    x :<| xs = x <| xs
{-# COMPLETE (:<|) #-}



infixr 5 :<<

-- | A unidirectional pattern synonym viewing the front of a non-empty
-- sequence.
pattern (:<<)    :: a -> LSeq 0 a -> LSeq n a
pattern x :<< xs <- (viewLSeq -> Just (x,xs))

-- | The empty sequence.
pattern EmptyL   :: LSeq n a
pattern EmptyL   <- (viewLSeq -> Nothing)

viewLSeq          :: LSeq n a -> Maybe (a,LSeq 0 a)
viewLSeq (LSeq s) = case S.viewl s of
                      S.EmptyL    -> Nothing
                      (x S.:< xs) -> Just (x,LSeq xs)


--------------------------------------------------------------------------------

-- | View of the right end of a sequence.
data ViewR n a where
  (:>) :: LSeq n a -> a -> ViewR (1 + n) a

infixl 5 :>

instance Semigroup (ViewR n a) where
  (xs :> x) <> (ys :> y) = LSeq ((toSeq xs S.|> x) <> toSeq ys) :> y

deriving instance Show a => Show (ViewR n a)
instance Functor (ViewR n) where
  fmap = Tr.fmapDefault
instance Foldable (ViewR n) where
  foldMap = Tr.foldMapDefault
instance Traversable (ViewR n) where
  traverse f (xs :> x) = (:>) <$> traverse f xs <*> f x
instance Eq a => Eq (ViewR n a) where
  s == s' = F.toList s == F.toList s'
instance Ord a => Ord (ViewR n a) where
  s `compare` s' = F.toList s `compare` F.toList s'

-- | \( O(1) \). Analyse the right end of a sequence.
viewr    :: LSeq (1 + n) a -> ViewR (1 + n) a
viewr xs = let ~(ys S.:> x) = S.viewr $ toSeq xs in LSeq ys :> x

viewr'    :: LSeq (1 + n) a -> (LSeq n a, a)
viewr' xs = let ~(ys S.:> x) = S.viewr $ toSeq xs in (LSeq ys, x)

infixl 5 :|>

-- | A bidirectional pattern synonym viewing the rear of a non-empty
-- sequence.
pattern (:|>)    :: forall n a. LSeq n a -> a -> LSeq (1 + n) a
pattern xs :|> x <- (viewr' -> (xs,x))
  where
    xs :|> x = xs |> x
{-# COMPLETE (:|>) #-}

--------------------------------------------------------------------------------

-- | Gets the first element of the LSeq
--
-- >>> head $ forceLSeq (Proxy :: Proxy 3) $ fromList [1,2,3]
-- 1
head           :: LSeq (1 + n) a -> a
head (x :<| _) = x

-- | Get the LSeq without its first element
-- -- >>> head $ forceLSeq (Proxy :: Proxy 3) $ fromList [1,2,3]
-- LSeq (fromList [2,3])
tail           :: LSeq (1 + n) a -> LSeq n a
tail (_ :<| s) = s

-- s = let (x :< _) = viewl s in x

-- | Get the last element of the LSeq
--
-- >>> last $ forceLSeq (Proxy :: Proxy 3) $ fromList [1,2,3]
-- 3
last           :: LSeq (1 + n) a -> a
last (_ :|> x) = x


-- | The sequence without its last element
--
-- >>> init $ forceLSeq (Proxy :: Proxy 3) $ fromList [1,2,3]
-- LSeq (fromList [1,2])
init           :: LSeq (1 + n) a -> LSeq n a
init (s :|> _) = s

-- testL = (eval (Proxy :: Proxy 2) $ fromList [1..5])

-- testL' :: LSeq 2 Integer
-- testL' = fromJust testL

-- test            :: Show a => LSeq (1 + n) a -> String
-- test (x :<| xs) = show x ++ show xs


--------------------------------------------------------------------------------

-- | Zips two equal length LSeqs
zipWith         :: (a -> b -> c) -> LSeq n a -> LSeq n b -> LSeq n c
zipWith f sa sb = LSeq $ S.zipWith f (toSeq sa) (toSeq sb)
