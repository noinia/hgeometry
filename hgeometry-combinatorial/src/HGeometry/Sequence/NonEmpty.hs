{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Sequence.NonEmpty
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Sequence.NonEmpty
  ( ViewL1(..)
  , ViewR1(..)
  , viewl1, viewr1
  , asViewL1, asViewR1
  , (|>>)
  , (<>>)
  -- , (<<>)
  , splitL1At
  , splitR1At
  ) where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Foldable1.WithIndex
import           Data.Functor.Apply
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Traversable
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Sequence
import           GHC.Exts (IsList(..))
import           GHC.Generics
import           HGeometry.Foldable.Util (HasFromFoldable1(..))

--------------------------------------------------------------------------------

-- | NonEmpty ViewL
data ViewL1 a = a :<< Seq a
  deriving stock (Show,Read,Eq,Ord,Functor,Traversable,Generic)

instance NFData a => NFData (ViewL1 a)

type instance Index   (ViewL1 a) = Int
type instance IxValue (ViewL1 a) = a

instance Ixed (ViewL1 a) where
  ix i f (x :<< xs)
    | i == 0    = (:<< xs) <$> f x
    | otherwise = (x :<<)  <$> ix (i-1) f xs
  {-# INLINE ix #-}

instance Foldable ViewL1 where
  foldMap f (x :<< xs) = f x <> foldMap f xs
  {-# INLINE foldMap #-}
  length (_ :<< xs) = 1 + length xs
  {-# INLINE length #-}
  null _ = False
  {-# INLINE null #-}

instance Foldable1 ViewL1 where
  foldMap1 f (a :<< as) = case foldMap (Just . f) as of
    Nothing -> f a
    Just b  -> f a <> b
  {-# INLINE foldMap1 #-}

instance Traversable1 ViewL1 where
  traverse1 f (a :<< as) = (:<<) <$> f a <.*> traverse1Maybe f as
  {-# INLINE traverse1 #-}

instance FunctorWithIndex Int ViewL1
instance FoldableWithIndex Int ViewL1
instance TraversableWithIndex Int ViewL1

instance Foldable1WithIndex Int ViewL1 where
  ifoldMap1 f (a :<< as) = case ifoldMap (\i -> Just . f (i+1)) as of
    Nothing -> f 0 a
    Just b  -> f 0 a <> b
  {-# INLINE ifoldMap1 #-}

-- instance Traversable1WithIndex Int ViewL1


instance Semigroup (ViewL1 a) where
  (a :<< as) <> (b :<< bs) = a :<< (as <> (b :<| bs))
  {-# INLINE (<>) #-}

instance HasFromFoldable1 ViewL1 where
  fromNonEmpty (a :| as) = a :<< fromList as
  {-# INLINE fromNonEmpty #-}

instance IsList (ViewL1 a) where
  type Item (ViewL1 a) = a
  toList = F.toList
  {-# INLINE toList #-}
  fromList = maybe (error "ViewL1 fromList; empty List") fromNonEmpty . NonEmpty.nonEmpty
  {-# INLINE fromList #-}

instance Reversing (ViewL1 a) where
  reversing (x :<< s) = viewl1 $ Sequence.reverse s :>> x

-- | Try to parse a Seq into a ViewL1
asViewL1 :: Seq a -> Maybe (ViewL1 a)
asViewL1 = \case
  x :<| xs -> Just (x :<< xs)
  _        -> Nothing

--------------------------------------------------------------------------------

-- | NonEmpty ViewR
data ViewR1 a = Seq a :>> a
  deriving stock (Show,Read,Eq,Ord,Functor,Traversable,Generic)

instance NFData a => NFData (ViewR1 a)

type instance Index   (ViewR1 a) = Int
type instance IxValue (ViewR1 a) = a

instance Ixed (ViewR1 a) where
  ix i f (xs :>> x)
    | i == length xs = (xs :>>) <$> f x
    | otherwise      = (:>> x)  <$> ix i f xs
  {-# INLINE ix #-}

instance Foldable ViewR1 where
  foldMap f (xs :>> x) = foldMap f xs <> f x
  {-# INLINE foldMap #-}
  length (xs :>> _) = 1 + length xs
  {-# INLINE length #-}
  null _ = False
  {-# INLINE null #-}

instance Foldable1 ViewR1 where
  foldMap1 f (as :>> a) = case foldMap (Just . f) as of
    Nothing -> f a
    Just b  -> b <> f a
  {-# INLINE foldMap1 #-}

instance Traversable1 ViewR1 where
  traverse1 f (as :>> a) = (:>>) <$> traverse1Maybe f as <*.> f a
  {-# INLINE traverse1 #-}

instance FunctorWithIndex Int ViewR1
instance FoldableWithIndex Int ViewR1
instance TraversableWithIndex Int ViewR1

instance Foldable1WithIndex Int ViewR1 where
  ifoldMap1 f (as :>> a) = case ifoldMap (\i -> Just . f i) as of
    Nothing -> f 0 a
    Just b  -> b <> f (length as) a
  {-# INLINE ifoldMap1 #-}

-- instance Traversable1WithIndex Int ViewR1


instance Semigroup (ViewR1 a) where
  (as :>> a) <> (bs :>> b) = ((as :|> a) <> bs) :>> b
  {-# INLINE (<>) #-}

instance HasFromFoldable1 ViewR1 where
  fromNonEmpty (a :| as) = case fromNonEmpty <$> NonEmpty.nonEmpty as of
                             Nothing          -> Sequence.empty :>> a
                             Just (as' :>> x) -> (a :<| as') :>> x
  {-# INLINE fromNonEmpty #-}

instance IsList (ViewR1 a) where
  type Item (ViewR1 a) = a
  toList = F.toList
  {-# INLINE toList #-}
  fromList = maybe (error "ViewR1 fromList; empty List") fromNonEmpty . NonEmpty.nonEmpty
  {-# INLINE fromList #-}

instance Reversing (ViewR1 a) where
  reversing (s :>> x) = viewr1 $ x :<< Sequence.reverse s


-- | snoc an element to the right
(|>>) :: ViewR1 a -> a -> ViewR1 a
(ys :>> y) |>> x = (ys :|> y) :>> x

infixl 5 |>>
infixl 5 <>>

-- | Append a Sequence to a ViewR1
(<>>)               :: ViewR1 a -> Seq a -> ViewR1 a
l@(ys :>> y) <>> rs = case Sequence.viewr rs of
                        Sequence.EmptyR   -> l
                        rs' Sequence.:> r -> (ys <> (y :<| rs')) :>> r




-- | View the leftmost element
viewl1            :: ViewR1 a -> ViewL1 a
viewl1 (xs :>> r) = case Sequence.viewl xs of
                      Sequence.EmptyL   -> r :<< Sequence.empty
                      l Sequence.:< mid -> l :<< (mid :|> r)

-- | View the rightmost element
viewr1            :: ViewL1 a -> ViewR1 a
viewr1 (l :<< ls) = case Sequence.viewr ls of
                      Sequence.EmptyR   -> Sequence.empty :>> l
                      mid Sequence.:> r -> (l :<| mid)    :>> r

-- | Try to parse a Seq into a ViewLR
asViewR1 :: Seq a -> Maybe (ViewR1 a)
asViewR1 = \case
  xs :|> x -> Just (xs :>> x)
  _        -> Nothing

--------------------------------------------------------------------------------

-- | Given an index i, and a viewL1 s, attempts to split s at index i. Returns nothing if
-- the index is out of range.
splitL1At             :: Int -> ViewL1 a -> Maybe (Seq a, a, Seq a)
splitL1At i (x :<< s) = clampRange i (x <| s) <&> \s' -> case Sequence.splitAt i s' of
  (pref, y :<| suff) -> (pref, y, suff)
  _                  -> error "splitL1At: absurd"

-- | Given an index i, and a viewL1 s, attempts to split s at index i. Returns nothing if
-- the index is out of range.
splitR1At             :: Int -> ViewR1 a -> Maybe (Seq a, a, Seq a)
splitR1At i (s :>> x) = clampRange i (s |> x) <&> \s' -> case Sequence.splitAt i s' of
  (pref, y :<| suff) -> (pref, y, suff)
  _                  -> error "splitR1At: absurd"

-- | Helper to make sure the index is within range.
clampRange :: Foldable t => Int -> t a -> Maybe (t a)
clampRange i s
  | i < 0           = Nothing
  | i >= F.length s = Nothing
  | otherwise       = Just s

--------------------------------------------------------------------------------





-- class CanCons s t a | s -> a, t -> a where
--   -- | This class provides a way to attach elements on the left side of a structure in a
--   -- flexible manner.
--   cons1 :: a -> s -> t

-- class CanUncons s t a | s -> a, t -> a where
--   -- | This class provides a way to detach elements on the left side of a structure in a
--   -- flexible manner.
--   uncons1 :: t -> (a, s)

-- class CanSnoc s t a | s -> a, t -> a where
--   -- | This class provides a way to attach elements on the left side of a structure in a
--   -- flexible manner.
--   snoc1 :: s -> a -> t

-- class CanUnsnoc s t a | s -> a, t -> a where
--   -- | This class provides a way to detach elements on the left side of a structure in a
--   -- flexible manner.
--   unsnoc1 :: t -> (s, a)

-- instance CanCons (Seq a) (ViewL1 a) a where
--   cons1 = (:<<)

-- instance CanCons (ViewL1 a) (ViewL1 a) a where
--   cons1 x (y :<< ys) = x :<< (y :<| ys)

-- instance CanUncons (Seq a) (ViewL1 a) a where
--   uncons1 (a :<< as) = (a, as)

-- class Cons1 s t a | s -> a
--                   , s -> t where
--   -- | This class provides a way to attach or detach elements on the left side of a
--   -- structure in a flexible manner.
--   _Cons1 :: Iso' s (a, t)

-- instance Cons1 (ViewL1 a) (Seq a) a where
--   _Cons1 = iso (\(a :<< as) -> (a, as)) (uncurry (:<<))

-- instance Cons1 (ViewR1 a) (Seq a) a where
--   _Cons1 = iso (\(as :>> r) -> case Sequence.viewl as of
--                                  Sequence.EmptyL     -> (r, Sequence.empty)
--                                  (l Sequence.:< mid) -> (l, mid :|> r))
--                (\(l,as) -> case Sequence.viewr as of
--                              Sequence.EmptyR     -> Sequence.empty :>> l
--                              (mid Sequence.:> r) -> (l :<| mid) :>> r)



-- instance Cons (ViewR1 a) (Identity (Seq b)) a b where
--   _Cons = prism (\(b, Identity seqB) -> Identity (b :<| seqB))
--                 (\(as :>> a) -> case Sequence.viewl as of
--                                   Sequence.EmptyL            -> (a, _)
--                                   (x Sequence.EmptyL.:<| s') -> _
--                 )


  -- foo bar
  --   where
  --     foo :: (b, ViewR1 b) -> ViewR1 b

  --     bar :: Identity (Seq a) -> Either (ViewR1 b) a

  -- (\(b, t :|> b') -> )


  -- (\(b,Identity t) -> b :<| t) (\(a :<< _) -> Left a)

-- instance Snoc (ViewR1 a) (Identity (Seq b)) a b where
--   _Snoc = prism (\(Identity t,b) -> t :|> b) (\(_ :>> a) -> Left a)




-- xs |>> x



-- class Cons1 s t a b | s -> a
--                     , t -> b
--                     , s b  -> t
--                     , t a  -> s  where
--   -- | This class provides a way to attach or detach elements on the left side of a
--   -- structure in a flexible manner.
--   _Cons1 :: Iso s t (a, Maybe s) (b, Maybe t)


-- cons1 :: Cons1 s t s' t' a b =>



-- instance Cons1 (ViewL1 a) (ViewL1 b) a b where
--   _Cons1 = iso (\(x :<< xs) -> (x,xs)

--                )





--            (uncurry (:<<))
--   {-# INLINE _Cons1 #-}

-- instance Cons1 (ViewR1 a) (ViewR1 b) (Seq a) (Seq b) a b where
--   _Cons1 = iso (\(xs :>> l) -> case Sequence.viewl xs of
--                                  Sequence.EmptyL     -> (l,xs)
--                                  (f Sequence.:< xs') -> (f,xs' :|> l))
--                (\(f,xs) -> case Sequence.viewr xs of
--                              Sequence.EmptyR     -> xs :>> f
--                              (xs' Sequence.:> l) -> (f :<| xs') :>> l)
--   {-# INLINE _Cons1 #-}
