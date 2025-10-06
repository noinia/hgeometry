{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Cyclic
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Representing Cyclic Sequences
--
--------------------------------------------------------------------------------
module HGeometry.Cyclic
  ( Cyclic(..)
  , HasDirectedTraversals(..)
  , ShiftedEq(..)

  , groupWith

  , withCyclicSuccessor
  , withCyclicPredecessor
  , withCyclicNeighbours


  , iWithCyclicSuccessor
  , iWithCyclicPredecessor
  , iWithCyclicNeighbours
  , V2(..)
  ) where

--------------------------------------------------------------------------------

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>), MaybeApply(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust)
import           Data.Semigroup.Traversable.Class (sequence1)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.NonEmpty as NV
import           GHC.Generics (Generic)
import           HGeometry.Foldable.Util
import           HGeometry.Sequence.NonEmpty
import           HGeometry.StringSearch.KMP (isSubStringOf)
import           HGeometry.Vector.NonEmpty.Util ()
import           Linear.V2 (V2(..))

--------------------------------------------------------------------------------

-- | A cyclic sequence type
newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable,Foldable1,NFData,Eq,ToJSON,FromJSON)
 deriving stock (Generic,Show)
-- not sure if we want this Eq instance or not .


instance Traversable1 v => Traversable1 (Cyclic v) where
  traverse1 f (Cyclic v) = Cyclic <$> traverse1 f v
instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v


instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

instance HasFromFoldable1 v => HasFromFoldable1 (Cyclic v)  where
  fromFoldable1 = Cyclic . fromFoldable1
  fromNonEmpty  = Cyclic . fromNonEmpty

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n  = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

instance Reversing (v a) => Reversing (Cyclic v a) where
  reversing (Cyclic v) = Cyclic (reversing v)

-- | Class that models that some type has a cyclic traversal starting
-- from a particular index.
class HasDirectedTraversals v where
  -- | A rightward-traversal over all elements starting from the given one. Indices are
  -- taken modulo the length.
  --
  -- running time : \(O(n)\)
  traverseRightFrom          :: Index (v a) -> IndexedTraversal1' (Index (v a)) (v a) a

  -- | A rightward-traversal over all elements starting from the given one. Indices are
  -- taken modulo the length.
  --
  -- running time : \(O(n)\)
  traverseLeftFrom          :: Index (v a) -> IndexedTraversal1' (Index (v a)) (v a) a

instance HasDirectedTraversals v => HasDirectedTraversals (Cyclic v) where
  traverseRightFrom s paFa (Cyclic v) = Cyclic <$> traverseRightFrom s paFa v
  traverseLeftFrom  s paFa (Cyclic v) = Cyclic <$> traverseLeftFrom  s paFa v

instance HasDirectedTraversals NonEmpty where
  traverseRightFrom i = \paFb xs -> let ((pref,x,suff),i')   = splitNonEmptyAt i xs
                                        paFb'                = wrapMaybeApply paFb
                                        combine (y :| sa) sb = y :| (sa <> sb)
                                    in combine <$>  reindexed (+i') traversed1 paFb (x :| suff)
                                               <.*> itraversed paFb' pref

  traverseLeftFrom i = \paFb xs -> let ((pref,x,suff),i') = splitNonEmptyAt i xs
                                       paFb'              = wrapMaybeApply paFb
                                       combine y sa sb    = foldr NonEmpty.cons (y:|sb) sa
                                   in
    combine <$>  indexed paFb i' x
            <.*> backwards itraversed paFb' pref
            <.*> backwards (reindexed (\j -> 1 + i' +j) itraversed) paFb' suff

-- | Given an index i, and a NonEmpty xs splits xs at index i `mod` n.
-- returns the splitted list at index i, also returns the index i `mod` n.
splitNonEmptyAt       :: Int -> NonEmpty a -> (([a],a,[a]),Int)
splitNonEmptyAt i xs0 = case go 0 xs0 of
                          Left n    -> let i' = (i+n) `mod` n in (go1 i' xs0, i')
                          Right res -> (res,i)
  where
    go l (x :| xs) | i == l    = Right ([],x,xs)
                   | otherwise = case go (l+1) <$> NonEmpty.nonEmpty xs of
                                   Nothing                    -> Left (l+1)
                                   Just (Right (pref,y,suff)) -> Right (x:pref,y,suff)
                                   Just left                  -> left
    go1 j (x :| xs) | j == 0    = ([],x,xs)
                    | otherwise = case go1 (j-1) <$> NonEmpty.nonEmpty xs of
                                    Nothing            -> error "splitNonEmptyAt. absurd"
                                    Just (pref,y,suff) -> (x:pref,y,suff)


instance HasDirectedTraversals NV.NonEmptyVector where
  traverseRightFrom s paFa v = traverseByOrder indices' paFa v
    where
      n        = F.length v
      indices' = NonEmpty.fromList [s..(s+n-1)]

  traverseLeftFrom s paFa v = traverseByOrder indices' paFa v
    where
      n        = F.length v
      indices' = NonEmpty.fromList [s,s-1..(s-n+1)]

-- | Helper to build traversal1's
wrapMaybeApply :: (Indexable Int p, Apply f) => p a (f b) -> p a (MaybeApply f b)
wrapMaybeApply = rmap (MaybeApply . Left)


instance HasDirectedTraversals ViewL1 where
  traverseRightFrom i = \paFb xs -> let i'    = i `mod` F.length xs
                                        paFb' = wrapMaybeApply paFb
                                        combine (x :<< sa) sb = x :<< (sa <> sb)
                                    in case splitL1At i' xs of
      Nothing            -> traversed1 paFb xs
      Just (pref,x,suff) -> combine <$>  reindexed (+i') traversed1 paFb (x :<< suff)
                                    <.*> itraversed paFb' pref

  traverseLeftFrom i = \paFb xs ->
                         let i'    = i `mod` F.length xs
                             paFb' = wrapMaybeApply paFb
                             combine r1 rs = viewl1 $ r1 <>> rs
                         in case splitL1At i' xs of
      Nothing            -> backwards traversed1 paFb xs
      Just (pref,x,suff) -> combine <$>  backwards traversed1 paFb (pref :>> x)
                                    <.*> backwards (reindexed (\j -> 1 + i' + j) itraversed)
                                                   paFb' suff

-- | traverse the vector in the given order
traverseByOrder                 :: NonEmpty.NonEmpty Int
                                -> IndexedTraversal1' Int (NV.NonEmptyVector a) a
traverseByOrder indices' paFa v = build <$> xs
  where
    n = F.length v
    xs = traverse1 (\i' -> let i = i' `mod` n
                           in (i,) <$> indexed paFa i (v NV.! i)
                   ) indices'
    build ys = NV.unsafeFromVector $ V.create $ do
                   v' <- MV.unsafeNew n
                   forM_ ys $ \(i,y) ->
                     MV.write v' i y
                   pure v'

-- | Test if the circular list is a cyclic shift of the second
-- list.
--
-- Running time: \(O(n+m)\), where \(n\) and \(m\) are the sizes of
-- the lists.
isShiftOfI         :: (Eq a, Foldable1 v) => Cyclic v a -> Cyclic v a -> Bool
xs `isShiftOfI` ys = let twice zs     = let zs' = leftElements zs in zs' <> zs'
                         once         = leftElements
                         leftElements = NV.fromNonEmpty . toNonEmpty
                         check as bs  = isJust $ once as `isSubStringOf` twice bs
                     in length xs == length ys && check xs ys

--------------------------------------------------------------------------------

-- | Class for types that have an Equality test up to cyclic shifts
class ShiftedEq t where
  -- | The type of the elements stored in this cyclic container.
  type ElemCyclic t
  -- | Given a and b, test if a is a shifted version of the other.
  isShiftOf :: Eq (ElemCyclic t) => t -> t -> Bool

  -- TODO: It may be nice to have a version that actually returns the index, if it is a
  -- shift.

instance Foldable1 v => ShiftedEq (Cyclic v a) where
  type ElemCyclic (Cyclic v a) = a
  -- ^ runs in linear time in the inputs.
  isShiftOf = isShiftOfI


--------------------------------------------------------------------------------
-- * Cyclic Traversals With Neighbours


-- | A traversal that associates every elemnt with its successor
withCyclicSuccessor :: forall cyclic a b. Traversable1 cyclic
                    => Traversal1 (cyclic a) (cyclic b) (a,a) b
withCyclicSuccessor = \f xs -> let x0 :| xs' = toNonEmpty xs
                               in sequence1 $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs

-- | An indexed version of 'withCyclicSuccessor'
iWithCyclicSuccessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                     => IndexedTraversal1 (i,i) (cyclic a) (cyclic b) (a,a) b
iWithCyclicSuccessor = conjoined withCyclicSuccessor (theITrav . indexed)
  where
    theITrav      :: Apply f => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let x0 :| xs' = toNonEmpty $ imap ((,)) xs
                    in sequence1 $ izipWithList (\(j,s) i x -> f (i,j) (x,s)) (xs' <> [x0]) xs

-- | A traversal that associates every elemnt with its predecessor
withCyclicPredecessor :: forall cyclic a b. Traversable1 cyclic
                      => Traversal1 (cyclic a) (cyclic b) (a,a) b
withCyclicPredecessor = \f xs -> let xs' = F.toList xs
                                 in sequence1 $ zipWithList (\p x -> f (p,x)) (xs^.last1 : xs') xs


-- | An indexed traversal that associates every elemnt with its predecessor
iWithCyclicPredecessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                       => IndexedTraversal1 (i,i) (cyclic a) (cyclic b) (a,a) b
iWithCyclicPredecessor = conjoined withCyclicPredecessor (theITrav . indexed)
  where
    theITrav      :: Apply f => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs' = imap ((,)) xs
                    in sequence1 $
                       izipWithList (\(i,p) j x -> f (i,j) (p,x)) (xs'^.last1 : F.toList xs') xs

-- | Traverse a cyclic structure together with both its neighbors
withCyclicNeighbours :: forall cyclic a b. Traversable1 cyclic
                     => Traversal1 (cyclic a) (cyclic b) (a, V2 a) b
withCyclicNeighbours = \f xs -> let x0 :| xs' = toNonEmpty xs
                                    ns        = zipWith V2 (xs^.last1 : x0 : xs') (xs' <> [x0])
                                in sequence1 $ zipWithList (\s x -> f (x,s)) ns xs

-- | An indexed traversal that associates every elemnt with its neighbours
iWithCyclicNeighbours :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                      => IndexedTraversal1 (i,V2 i) (cyclic a) (cyclic b) (a,V2 a) b
iWithCyclicNeighbours = conjoined withCyclicNeighbours (theITrav . indexed)
  where
    theITrav      :: Apply f
                  => ((i,V2 i) -> (a,V2 a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs'       = imap ((,)) xs
                        y0 :| ys' = toNonEmpty xs'
                        ns        = zipWith V2 (xs'^.last1 : F.toList xs') (ys' <> [y0])
                    in sequence1 $ izipWithList (\(V2 (h,p) (j,s)) i x
                                                  -> f (i, V2 h j) (x,V2 p s)) ns xs
-- unfortuantely, we can't use Vector2 because that would introduce a cyclic dependency :(

----------------------------------------
-- Helper functions to implement the traversals above


-- | pre: the f and the list have the same size
zipWithList      :: forall f a b c. Traversable f
                 => (a -> b -> c) -> [a] -> f b -> f c
zipWithList f xs = flip evalState xs . traverse go
  where
    go   :: b -> State [a] c
    go y = get >>= \case
      []      -> error "zipWithList. precondition failed, too few a's"
      (x:xs') -> do put xs'
                    pure $ f x y

-- | pre: the f and the list have the same size
izipWithList      :: forall f i a b c. TraversableWithIndex i f
                 => (a -> i -> b -> c) -> [a] -> f b -> f c
izipWithList f xs = flip evalState xs . itraverse go
  where
    go     :: i -> b -> State [a] c
    go i y = get >>= \case
      []      -> error "izipWithList. precondition failed, too few a's"
      (x:xs') -> do put xs'
                    pure $ f x i y


--------------------------------------------------------------------------------


-- | Groups the elements of a cyclic. Note in particular that this may join the first and
-- last group, thereby changing the indices of the individual elements.
--
-- the items are reported in the same order as before.
groupWith      :: (Foldable1 cyclic, Eq b)
               => (a -> b) -> cyclic a -> Cyclic NonEmpty (b, NonEmpty a)
groupWith f xs = Cyclic $ case foldrMap1 initialize compute xs of
    OneRun y run  -> NonEmpty.singleton (y, run)
    MoreRuns y current completed b lastRun
      | b == y    -> (b, lastRun <> current) :| completed -- first and last are the same
      | otherwise -> (b, lastRun) :| (y,current) : completed
       -- our last run differs from the current run. Since the thing is cyclic, just
       -- use the last run as the first run though.
  where
    -- we either have a Single run or Multiple runs once we detect
    -- that we have more than one run, any future values will just be
    -- Multiple runs
    initialize x = OneRun (f x) (NonEmpty.singleton x)
    compute x = \case
        OneRun y current
          | b == y    -> OneRun y (x NonEmpty.<| current)
          | otherwise -> MoreRuns b (NonEmpty.singleton x) [] y current
                         -- start a new current run
        MoreRuns y current completed lastB lastRun
         | b == y    -> MoreRuns y (x NonEmpty.<| current) completed               lastB lastRun
         | otherwise -> MoreRuns b (NonEmpty.singleton x)  ((y,current):completed) lastB lastRun
      where
        b = f x

data Runs b a = OneRun   !b (NonEmpty a)
              | MoreRuns !b (NonEmpty a) [(b,NonEmpty a)] !b (NonEmpty a)
              -- the first !b (NonEmpty a) is the current run
              -- the last !b (NonEmpty a) is the last run






{- Alternative implementation; this one seems to be slower though

groupWith      :: (Foldable1 cyclic, Functor cyclic, Eq b)
               => (a -> b) -> cyclic a -> Cyclic NonEmpty (b, NonEmpty a)
groupWith f xs = Cyclic . fmap g $ case unsnoc1 ys of
    (run1:rest, lastRun) | f' run1 == f' lastRun -> lastRun <> run1 :| rest
    _                                            -> ys
  where
    f'      = fst . NonEmpty.head
    g run   = (f' run, snd <$> run)
    ys      = NonEmpty.groupWith1 fst . toNonEmpty . fmap (\x -> (f x,x)) $ xs
    unsnoc1 = foldrMap1 ([],) (\x (xs',l) -> (x:xs', l))
-}





-- groupWith      :: (Foldable1 cyclic, Eq b)
--                => (a -> b) -> cyclic a -> Cyclic NonEmpty (b, NonEmpty a)




-- groupWith f xs = Cyclic $ case foldrMap1 initialize compute xs of
--     Left res      -> NonEmpty.singleton res
--     Right ((x, first), res@((y, current) :| completed))
--       | x == y    -> (x, first <> current) :| completed
--       | otherwise -> (x, first) NonEmpty.<| res
--   where
--     initialize x = Left (f x, NonEmpty.singleton x)
--     compute x = \case
--         Left (y,current)
--           | b == y    -> Left (y, x NonEmpty.<| current)
--           | otherwise -> Right ((y,current), NonEmpty.singleton (b, NonEmpty.singleton x))
--         Right (first, res@((y,current):|completed))
--           | b == y    -> Right (first, (y, x NonEmpty.<| current) :| completed)
--           | otherwise -> Right (first, (b, NonEmpty.singleton x) NonEmpty.<| res)
--       where
--         b = f x
