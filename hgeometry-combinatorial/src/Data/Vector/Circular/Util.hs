{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vector.Circular.Util where

import           Algorithms.StringSearch.KMP (isSubStringOf)
import           Control.Lens
import           Data.Ext
import           Data.Maybe
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import           Data.Vector.Circular as CV
import qualified Data.Vector.NonEmpty as NV
import           Test.QuickCheck (Arbitrary (..), NonEmptyList (..))


-- FIXME: Upstream this to the non-empty vector library?
instance Foldable1 NV.NonEmptyVector

-- | Access the ith item in the CircularVector (w.r.t the rotation) as a lens
item   :: Int -> Lens' (CircularVector a) a
item i = lens (`CV.index` i) (\s x -> unsafeFromVector (toVector s V.// [(i,x)]))

-- | All elements, starting with the focus, going to the right
--
-- >>> rightElements $ unsafeFromList [3,4,5,1,2]
-- [3,4,5,1,2]
rightElements :: CircularVector a -> NV.NonEmptyVector a
rightElements = toNonEmptyVector

-- | All elements, starting with the focus, going to the left
--
-- >>> leftElements $ unsafeFromList [3,4,5,1,2]
-- [3,2,1,5,4]
leftElements :: CircularVector a -> NV.NonEmptyVector a
leftElements v = NV.generate1 (length v) (CV.index v . negate)

-- | Finds an element in the CircularVector
--
-- >>> findRotateTo (== 3) $ unsafeFromList [1..5]
-- Just (CircularVector {vector = [1,2,3,4,5], rotation = 2})
-- >>> findRotateTo (== 7) $ unsafeFromList [1..5]
-- Nothing
findRotateTo   :: (a -> Bool) -> CircularVector a -> Maybe (CircularVector a)
findRotateTo p (CircularVector v _rot) = CircularVector v <$> NV.findIndex p v

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

instance Arbitrary a => Arbitrary (CircularVector a) where
  arbitrary = unsafeFromList <$> (getNonEmpty <$> arbitrary)

map :: (a -> b) -> CircularVector a -> CircularVector b
map fn (CircularVector ne rot) = CircularVector (NV.map fn ne) rot

-- | label the circular vector with indices, starting from zero at the
-- current focus, going right.
--
-- Running time: \(O(n)\)
withIndicesRight                      :: CircularVector a -> CircularVector (Int :+ a)
withIndicesRight (CircularVector v s) = CircularVector v' s
  where
    n  = length v
    v' = NV.imap (\i x -> ((i-s) `mod` n) :+ x) v

imap :: (Int -> a -> b) -> CircularVector a -> CircularVector b
imap f = fromVector . NV.imap f . toNonEmptyVector
