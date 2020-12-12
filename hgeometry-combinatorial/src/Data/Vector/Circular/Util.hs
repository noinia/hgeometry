{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vector.Circular.Util where

import           Algorithms.StringSearch.KMP (isSubStringOf)
import           Control.DeepSeq
import           Control.Lens
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe
import           Data.Semigroup.Foldable
import qualified Data.Vector                 as V
import           Data.Vector.Circular        as CV
import qualified Data.Vector.NonEmpty        as NV
import           GHC.Generics                (Generic)

deriving instance Generic (CircularVector a)

instance NFData a => NFData (CircularVector a)

instance Traversable CircularVector where
  traverse f (CircularVector v rot) =
    CircularVector <$> traverse f v <*> pure rot

instance Foldable1 NV.NonEmptyVector

-- type instance Index (CircularVector a) = Int
-- type instance IxValue (CircularVector a) = a

-- instance Ixed (CircularVector a) where
--   ix i f v = f (CV.index v i) <&> \a -> unsafeFromVector (toVector v V.// [(i,a)])

item   :: Int -> Lens' (CircularVector a) a
item i = lens (`CV.index` i) (\s x -> unsafeFromVector (toVector s V.// [(i,x)]))

-- FIXME: Will stream fusion make this efficient? Try expression this function
-- with Data.Vector.Fusion.Bundle.
zipWith :: (a -> b -> c) -> CircularVector a -> CircularVector b -> CircularVector c
zipWith f a b = unsafeFromVector $ V.zipWith f (toVector a) (toVector b)

zipWith3 :: (a -> b -> c -> d) -> CircularVector a -> CircularVector b -> CircularVector c
  -> CircularVector d
zipWith3 f a b c = fromVector $ NV.zipWith3 f (toNonEmptyVector a) (toNonEmptyVector b) (toNonEmptyVector c)

reverseDirection :: CircularVector a -> CircularVector a
reverseDirection = unsafeFromVector . V.reverse . toVector

-- minIndexBy :: (a -> a -> Ordering) -> CircularVector a -> Int
-- minIndexBy fn (CircularVector v rot) = (NV.minIndexBy fn v - rot) `mod` NV.length v

toNonEmptyVector :: CircularVector a -> NV.NonEmptyVector a
toNonEmptyVector v = NV.generate1 (length v) (CV.index v)

rotateToMinimumBy :: (a -> a -> Ordering) -> CircularVector a -> CircularVector a
rotateToMinimumBy fn (CircularVector v _rot) =
  CircularVector v (NV.minIndexBy fn v)

rotateToMaximumBy :: (a -> a -> Ordering) -> CircularVector a -> CircularVector a
rotateToMaximumBy fn (CircularVector v _rot) =
  CircularVector v (NV.maxIndexBy fn v)

rightElements :: CircularVector a -> NV.NonEmptyVector a
rightElements = toNonEmptyVector

leftElements :: CircularVector a -> NV.NonEmptyVector a
leftElements v = NV.generate1 (length v) (\i -> CV.index v (length v-1-i))

findRotateTo   :: (a -> Bool) -> CircularVector a -> Maybe (CircularVector a)
findRotateTo p (CircularVector v _rot) = CircularVector v <$> NV.findIndex p v

-- Delete once circular-vector has been fixed.
safeToNonEmpty :: CircularVector a -> NonEmpty.NonEmpty a
safeToNonEmpty = NV.toNonEmpty . toNonEmptyVector

-- | Test if the circular list is a cyclic shift of the second
-- list.
--
-- Running time: \(O(n+m)\), where \(n\) and \(m\) are the sizes of
-- the lists.
isShiftOf         :: Eq a => CircularVector a -> CircularVector a -> Bool
xs `isShiftOf` ys = let twice zs    = let zs' = leftElements zs in zs' <> zs'
                        once        = leftElements
                        check as bs = isJust $ once as `isSubStringOf` twice bs
                    in length xs == length ys && check xs ys
