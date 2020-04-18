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
module Data.LSeq( LSeq
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
                , pattern (:<|)

                , pattern (:<<)
                , pattern EmptyL

                , ViewR(..)
                , viewr
                , pattern (:|>)

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


empty :: LSeq 0 a
empty = LSeq S.empty

(<|) :: a -> LSeq n a -> LSeq (1 + n) a
x <| xs = LSeq (x S.<| toSeq xs)

(|>)    :: LSeq n a -> a -> LSeq (1 + n) a
xs |> x = LSeq (toSeq xs S.|> x)

infixr 5 <|
infixl 5 |>

(><) :: LSeq n a -> LSeq m a -> LSeq (n + m) a
xs >< ys = LSeq (toSeq xs <> toSeq ys)

infix 5 ><


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
sa `append` sb = LSeq $ (toSeq sa) <> toSeq sb

--------------------------------------------------------------------------------

-- | get the element with index i, counting from the left and starting at 0.
-- O(log(min(i,n-i)))
index     :: LSeq n a -> Int -> a
index s i = s^?!ix i

adjust       :: (a -> a) -> Int -> LSeq n a -> LSeq n a
adjust f i s = s&ix i %~ f


partition   :: (a -> Bool) -> LSeq n a -> (LSeq 0 a, LSeq 0 a)
partition p = bimap LSeq LSeq . S.partition p . toSeq

mapWithIndex   :: (Int -> a -> b) -> LSeq n a -> LSeq n b
mapWithIndex f = wrapUnsafe (S.mapWithIndex f)

take   :: Int -> LSeq n a -> LSeq 0 a
take i = wrapUnsafe (S.take i)

drop   :: Int -> LSeq n a -> LSeq 0 a
drop i = wrapUnsafe (S.drop i)


unstableSortBy   :: (a -> a -> Ordering) -> LSeq n a -> LSeq n a
unstableSortBy f = wrapUnsafe (S.unstableSortBy f)

unstableSort :: Ord a => LSeq n a -> LSeq n a
unstableSort = wrapUnsafe (S.unstableSort)


wrapUnsafe :: (S.Seq a -> S.Seq b) -> LSeq n a -> LSeq m b
wrapUnsafe f = LSeq . f . toSeq

--------------------------------------------------------------------------------

fromSeq :: S.Seq a -> LSeq 0 a
fromSeq = LSeq

fromList :: Foldable f => f a -> LSeq 0 a
fromList = LSeq . S.fromList . F.toList

fromNonEmpty :: NonEmpty.NonEmpty a -> LSeq 1 a
fromNonEmpty = LSeq . S.fromList . F.toList


--------------------------------------------------------------------------------

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


viewl :: LSeq (1 + n) a -> ViewL (1 + n) a
viewl xs = let ~(x S.:< ys) = S.viewl $ toSeq xs in x :< LSeq ys

viewl'    :: LSeq (1 + n) a -> (a, LSeq n a)
viewl' xs = let ~(x S.:< ys) = S.viewl $ toSeq xs in (x,LSeq ys)

infixr 5 :<|

pattern (:<|)    :: a -> LSeq n a -> LSeq (1 + n) a
pattern x :<| xs <- (viewl' -> (x,xs)) -- we need the coerce unfortunately
  where
    x :<| xs = x <| xs
{-# COMPLETE (:<|) #-}



infixr 5 :<<

pattern (:<<)    :: a -> LSeq 0 a -> LSeq n a
pattern x :<< xs <- (viewLSeq -> Just (x,xs))

pattern EmptyL   :: LSeq n a
pattern EmptyL   <- (viewLSeq -> Nothing)

viewLSeq          :: LSeq n a -> Maybe (a,LSeq 0 a)
viewLSeq (LSeq s) = case S.viewl s of
                      S.EmptyL    -> Nothing
                      (x S.:< xs) -> Just (x,LSeq xs)


--------------------------------------------------------------------------------

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

viewr    :: LSeq (1 + n) a -> ViewR (1 + n) a
viewr xs = let ~(ys S.:> x) = S.viewr $ toSeq xs in LSeq ys :> x

viewr'    :: LSeq (1 + n) a -> (LSeq n a, a)
viewr' xs = let ~(ys S.:> x) = S.viewr $ toSeq xs in (LSeq ys, x)

infixl 5 :|>

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
