{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.RangeTree where

import           Control.Lens
import           Data.BinaryTree (Measured(..))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.RangeTree.Measure
import qualified Data.Geometry.RangeTree.Generic as GRT
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Range
import           Data.Semigroup.Foldable
import           Data.Vector.Fixed.Cont (Peano, PeanoNum(..))
import           GHC.TypeLits


--------------------------------------------------------------------------------

newtype RangeTree d v p r = RangeTree (RT (Peano d) d v p r)
                          -- deriving (Show,Eq)



type RT i d v p r = GRT.RangeTree (Assoc i d v p r) (Leaf i d v p r) r


type family Assoc (i :: PeanoNum) d v p r where
  Assoc (S Z)     d v p r = v (Point d r :+ p)
  Assoc (S (S i)) d v p r = Assoc' (S i) d v p r

newtype Assoc' i d v p r = Assoc' { getAssoc :: Maybe (RT i d v p r) }



instance Semigroup (Assoc' i d v p r) => Monoid (Assoc' i d v p r) where
  mempty = Assoc' Nothing

instance ( Ord r, Arity d, MeasuredRT v, Semigroup (v (Point d r :+ p))
         ) => Semigroup (Assoc' (S Z) d v p r) where
  (Assoc' l) <> (Assoc' r) = createAssoc' $ merge (toList l) (toList r)
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



-- deriving instance (Show (v (Point d r :+ p)), Show r, Show p, Arity d) => Show (Assoc' 1 d v p r)
-- deriving instance (Eq (v (Point d r :+ p)),   Eq r,   Eq p,   Arity d) => Eq   (Assoc' 1 d v p r)

-- deriving instance (Show (Assoc' (i-1) d v p r)) => Show (Assoc' i d v p r)
-- deriving instance (Eq (v (Point d r :+ p)),   Eq r,   Eq p,   Arity d) => Eq   (Assoc' i d v p r)






type RangeTree1D d v p r =  GRT.RangeTree (v (Point d r :+ p)) (Leaf (S Z) d v p r) r

newtype Leaf (i :: PeanoNum) d v p r = Leaf [Point d r :+ p]
                                     deriving (Semigroup,Monoid)

deriving instance (Show r, Show p, Arity d) => Show (Leaf i d v p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (Leaf i d v p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (Leaf i d v p r)

--------------------------------------------------------------------------------

instance (MeasuredRT v, Semigroup (v (Point d r :+ p))
         ) => Measured (v (Point d r :+ p)) (Leaf (S Z) d v p r) where
  measure (Leaf pts) = measureRT pts

instance (MeasuredRT v, Semigroup (v (Point d r :+ p))
         , 2 <= d, Arity d, Ord r
         ) => Measured (Assoc' (S Z) d v p r) (Leaf (S (S Z)) d v p r) where
  measure (Leaf pts) = createAssoc . map (\p -> p^.core.yCoord :+ p) $ pts


create1DTree :: (Ord r, Measured (v (Point d r :+ p)) (Leaf (S Z) d v p r))
             => NonEmpty (r :+ (Point d r :+ p))
             -> RT (S Z) d v p r
create1DTree = GRT.createTree . fmap (&extra %~ Leaf . (:[]))

--------------------------------------------------------------------------------

type Assoc2 d v p r = Assoc' (S Z) d v p r

-- newtype Assoc2 d v p r = Assoc { getAssoc :: Maybe (RangeTree1D d v p r) }

-- deriving instance (Show (v (Point d r :+ p)), Show r, Show p, Arity d) => Show (Assoc2 d v p r)
-- deriving instance (Eq (v (Point d r :+ p)),   Eq r,   Eq p,   Arity d) => Eq   (Assoc2 d v p r)


-- | Creates an associated DS from a pre-sorted list of points
createAssoc' :: (Ord r, MeasuredRT v, Semigroup (v (Point d r :+ p)))
             => [r :+ Leaf (S Z) d v p r] -> Assoc' (S Z) d v p r
createAssoc' = Assoc' . fmap GRT.createTree' . NonEmpty.nonEmpty

createAssoc :: (Ord r, MeasuredRT v, Semigroup (v (Point d r :+ p)))
            => [r :+ (Point d r :+ p)] -> Assoc2 d v p r
createAssoc = Assoc' . fmap create1DTree . NonEmpty.nonEmpty





type RangeTree2D d v p r = GRT.RangeTree (Assoc2 d v p r) (Leaf (S (S Z)) d v p r) r

-- instance ( MeasuredRT v, Semigroup (v (Point d r :+ p)), 2 <= d, Arity d, Ord r
--          ) => Measured (Assoc2 d v p r) (Leaf 2 d v p r) where
--   measure (Leaf pts) = createAssoc . map (\p -> p^.core.yCoord :+ p) $ pts


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
