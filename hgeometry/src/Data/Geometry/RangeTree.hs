{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.RangeTree where

import           Control.Lens
import           Data.BinaryTree(Measured(..))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import qualified Data.Geometry.RangeTree.Generic as GRT
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Range
import           Data.Semigroup.Foldable
import           GHC.TypeLits


--------------------------------------------------------------------------------


type RangeTree1D d v p r =  GRT.RangeTree (v (Point d r :+ p)) (Leaf 1 d v p r) r

newtype Leaf i d v p r = Leaf [Point d r :+ p]
                       deriving (Semigroup,Monoid)

deriving instance (Show r, Show p, Arity d) => Show (Leaf i d v p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (Leaf i d v p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (Leaf i d v p r)

--------------------------------------------------------------------------------

class MeasuredRT v where
  measureRT :: [a] -> v a

--------------------------------------------------------------------------------

instance (MeasuredRT v, Semigroup (v (Point d r :+ p))
         ) => Measured (v (Point d r :+ p)) (Leaf 1 d v p r) where
  measure (Leaf pts) = measureRT pts


create1DTree :: (Ord r, Measured (v (Point d r :+ p)) (Leaf 1 d v p r))
             => NonEmpty (r :+ (Point d r :+ p)) -> RangeTree1D d v p r
create1DTree = GRT.createTree . fmap (&extra %~ Leaf . (:[]))

--------------------------------------------------------------------------------

newtype Assoc2 d v p r = Assoc { getAssoc :: Maybe (RangeTree1D d v p r) }

deriving instance (Show (v (Point d r :+ p)), Show r, Show p, Arity d) => Show (Assoc2 d v p r)
deriving instance (Eq (v (Point d r :+ p)),   Eq r,   Eq p,   Arity d) => Eq   (Assoc2 d v p r)


-- | Creates an associated DS from a pre-sorted list of points
createAssoc' :: (Ord r, MeasuredRT v, Semigroup (v (Point d r :+ p)))
            => [r :+ Leaf 1 d v p r] -> Assoc2 d v p r
createAssoc' = Assoc . fmap GRT.createTree' . NonEmpty.nonEmpty

createAssoc :: (Ord r, MeasuredRT v, Semigroup (v (Point d r :+ p)))
            => [r :+ (Point d r :+ p)] -> Assoc2 d v p r
createAssoc = Assoc . fmap create1DTree . NonEmpty.nonEmpty


instance (Ord r, Arity d, MeasuredRT v, Semigroup (v (Point d r :+ p)))
         => Monoid (Assoc2 d v p r) where
  mempty = Assoc Nothing

instance ( Ord r, Arity d, MeasuredRT v, Semigroup (v (Point d r :+ p))
         ) => Semigroup (Assoc2 d v p r) where
  (Assoc l) <> (Assoc r) = createAssoc' $ merge (toList l) (toList r)
    where
      toList = maybe [] (F.toList . GRT.toAscList)

merge :: (Ord r, Semigroup ps) => [r :+ ps] -> [r :+ ps] -> [r :+ ps]
merge = go
  where
    go []         bs = bs
    go as         [] = as
    go as@(a:as') bs@(b:bs') = case (a^.core) `compare` (b^.core) of
       LT -> a : go as' bs
       GT -> b : go as  bs'
       EQ -> (a^.core :+ (a^.extra) <> (b^.extra)) : go as' bs'






type RangeTree2D d v p r = GRT.RangeTree (Assoc2 d v p r) (Leaf 2 d v p r) r

instance ( MeasuredRT v, Semigroup (v (Point d r :+ p)), 2 <= d, Arity d, Ord r
         ) => Measured (Assoc2 d v p r) (Leaf 2 d v p r) where
  measure (Leaf pts) = createAssoc . map (\p -> p^.core.yCoord :+ p) $ pts


create2DTree :: (Ord r, MeasuredRT v, Semigroup (v (Point d r :+ p)), Arity d, 2 <= d, 1 <= d)
             => NonEmpty (Point d r :+ p) -> RangeTree2D d v p r
create2DTree = GRT.createTree . fmap (\p -> (p^.core.xCoord) :+ Leaf [p])

search   :: (Ord r, Monoid (v (Point d r :+ p)))
         => Vector 2 (Range r) -> RangeTree2D d v p r -> v (Point d r :+ p)
search r = mconcat . search' r

search'                 :: Ord r
                        => Vector 2 (Range r) -> RangeTree2D d v p r -> [v (Point d r :+ p)]
search' (Vector2 xr yr) = concatMap (queryAssoc yr) . GRT.search' xr

queryAssoc   :: Ord r => Range r -> Assoc2 d v p r -> [v (Point d r :+ p)]
queryAssoc r = maybe [] (GRT.search' r) . getAssoc


-- newtype RangeTree (d :: Nat) v p r = RangeTree (GRT.RangeTree v p r)



-- newtype Assoc v = Assoc v



-- newtype Leaf = LeafD () r

-- newtype AssocD v = AssocD (RangeTree (d - 1) v p r)


--------------------------------------------------------------------------------
instance MeasuredRT GRT.Report where
  measureRT = GRT.Report
--------------------------------------------------------------------------------

newtype Count a = Count { getCount :: Int } deriving (Show,Read,Eq,Ord)

instance MeasuredRT Count where
  measureRT = Count . length

instance Monoid (Count a) where
  mempty = Count 0

instance Semigroup (Count a) where
  (Count l) <> (Count r) = Count $ l + r

--------------------------------------------------------------------------------

data And l r a = And (l a) (r a) deriving (Show,Eq,Ord)

type (:*:) l r = And l r

instance (MeasuredRT l, MeasuredRT r) => MeasuredRT (l :*: r) where
  measureRT xs = And (measureRT xs) (measureRT xs)

instance (Semigroup (l a), Semigroup (r a)) => Semigroup ((l :*: r) a) where
  (And l r) <> (And l' r') = And (l <> l') (r <> r')


-- newtype All (ls :: [* -> *]) a = All (Map ls a)

-- type family Map (ls :: [* -> *]) (a :: *) where
--   Map '[]
