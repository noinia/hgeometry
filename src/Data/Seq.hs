{-# LANGUAGE ScopedTypeVariables #-}
module Data.Seq( LSeq
               , toSeq
               , empty
               , fromList
               , fromNonEmpty
               , fromSeq
               , toNonEmpty

               , (<|), (|>)
               , (><)
               , eval

               , index
               , adjust
               , partition
               , mapWithIndex
               , take
               , drop
               , unstableSortBy

               , ViewL(..)
               , viewl
               , pattern (:<|)

               , pattern (:<<)
               , pattern EmptyL

               , ViewR(..)
               , viewr
               , pattern (:|>)


               , promise
               ) where

import           Control.Lens ((%~), (&), (<&>), (^?), bimap)
import           Control.Lens.At (Ixed(..), Index, IxValue)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Semigroup
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import           GHC.TypeLits
import           Prelude hiding (drop,take)


-- | LSeq n a certifies that the sequence has *at least* n items
newtype LSeq (n :: Nat) a = LSeq { toSeq :: S.Seq a}
                          deriving (Show,Read,Eq,Ord,Foldable,Functor,Traversable)

instance Semigroup (LSeq n a) where
  (LSeq s) <> (LSeq s') = LSeq (s <> s')

instance Monoid (LSeq 0 a) where
  mempty = empty
  mappend = (<>)

type instance Index   (LSeq n a) = Int
type instance IxValue (LSeq n a) = a
instance Ixed (LSeq n a) where
  ix i f s@(LSeq xs)
    | 0 <= i && i < S.length xs = f (S.index xs i) <&> \x -> LSeq $ S.update i x xs
    | otherwise                 = pure s

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
promise :: LSeq m a -> LSeq n a
promise = LSeq . toSeq

toNonEmpty :: LSeq (1 + n) a -> NonEmpty.NonEmpty a
toNonEmpty = NonEmpty.fromList . F.toList


--------------------------------------------------------------------------------

-- | get the element with index i, counting from the left and starting at 0.
-- O(log(min(i,n-i)))
index     :: LSeq n a -> Int -> a
index s i = fromJust $ s^?ix i

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
instance Eq a => Eq (ViewL n a) where
  s == s' = F.toList s == F.toList s'
instance Ord a => Ord (ViewL n a) where
  s `compare` s' = F.toList s `compare` F.toList s'


viewl :: LSeq (1 + n) a -> ViewL (1 + n) a
viewl xs = let ~(x S.:< ys) = S.viewl $ toSeq xs in x :< LSeq ys

infixr 5 :<|

-- pattern (:<|)    :: a -> LSeq n a -> LSeq (1 + m) a
pattern x :<| xs <- (viewl -> x :< xs)
  where
    x :<| xs = x <| xs

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

viewr :: LSeq (1 + n) a -> ViewR (1 + n) a
viewr xs = let ~(ys S.:> x) = S.viewr $ toSeq xs in LSeq ys :> x


infixl 5 :|>

-- pattern (:|>) :: LSeq n a -> a -> LSeq (1 + n) a
pattern xs :|> x <- (viewr -> xs :> x)
  where
    xs :|> x = xs |> x

--------------------------------------------------------------------------------

-- testL = (eval (Proxy :: Proxy 2) $ fromList [1..5])

-- testL' :: LSeq 2 Integer
-- testL' = fromJust testL

-- test            :: Show a => LSeq (1 + n) a -> String
-- test (x :<| xs) = show x ++ show xs
