{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.RangeTree where

import           Control.Lens hiding (element)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import qualified Data.Geometry.RangeTree.Generic as GRT
import           Data.Geometry.RangeTree.Measure
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Measured.Class
import           Data.Proxy
import           Data.Range
import           Data.Semigroup.Foldable
import           Data.Vector.Fixed.Cont (Peano, PeanoNum(..))
import           GHC.TypeLits
import           Prelude hiding (last,init,head)

--------------------------------------------------------------------------------

type RangeTree d = RT d d

newtype RT i d v p r =
  RangeTree { _unRangeTree :: GRT.RangeTree (Assoc i d v p r) (Leaf i d v p r) r }

deriving instance (Show r, Show (Assoc i d v p r), Show (Leaf i d v p r)) => Show (RT i d v p r)
deriving instance (Eq r,   Eq   (Assoc i d v p r), Eq   (Leaf i d v p r)) => Eq   (RT i d v p r)

newtype Leaf i d v p r = Leaf { _getPts :: [Point d r :+ p]} deriving (Semigroup,Monoid)

deriving instance (Show r, Show p, Arity d) => Show (Leaf i d v p r)
deriving instance (Eq r, Eq p,     Arity d) => Eq   (Leaf i d v p r)


type family AssocT i d v p r where
  AssocT 1 d v p r = v (Point d r :+ p)
  AssocT 2 d v p r = Maybe (RT 1 d v p r)

newtype Assoc i d v p r = Assoc { unAssoc :: AssocT i d v p r }

deriving instance Show (AssocT i d v p r) => Show (Assoc i d v p r)
deriving instance Eq   (AssocT i d v p r) => Eq   (Assoc i d v p r)


type RTMeasure v d p r = (LabeledMeasure v, Semigroup (v (Point d r :+ p)))

instance RTMeasure v d p r => Semigroup (Assoc 1 d v p r) where
  (Assoc l) <> (Assoc r) = Assoc $ l <> r

instance (RTMeasure v d p r, Ord r, 1 <= d, Arity d) => Semigroup (Assoc 2 d v p r) where
  (Assoc l) <> (Assoc r) = Assoc . createRangeTree'' $ toList l <> toList r
    where
      toList = maybe [] (F.toList . toAscList)
      createRangeTree'' = fmap createRangeTree1 . NonEmpty.nonEmpty



instance (RTMeasure v d p r, Ord r, 1 <= d, Arity d) => Monoid (Assoc 2 d v p r) where
  mempty = Assoc Nothing

----------------------------------------

instance ( RTMeasure v d p r
         ) => Measured (Assoc 1 d v p r) (Leaf 1 d v p r) where
  measure (Leaf pts) = Assoc . labeledMeasure $ pts

instance ( RTMeasure v d p r, Ord r, 1 <= d, Arity d
         ) => Measured (Assoc 2 d v p r) (Leaf 2 d v p r) where
  measure (Leaf pts) = Assoc . createRangeTree'' $ pts
    where
      createRangeTree'' = fmap createRangeTree1 . NonEmpty.nonEmpty

----------------------------------------

createRangeTree' :: (Ord r, RTMeasure v d p r
                   -- , Arity d, Arity (d+1), d ~ (d' + 1), Arity d'
                   -- , Measured (Assoc d v p r) (Leaf d v p r)
                   )
                 => [Point d r :+ p] -> Maybe (RT i d v p r)
createRangeTree' = fmap createRangeTree . NonEmpty.nonEmpty


createRangeTree :: (Ord r, RTMeasure v d p r
                   -- , Arity d, Arity (d+1), d ~ (d' + 1), Arity d'
                   -- , Measured (Assoc d v p r) (Leaf d v p r)
                   )
                => NonEmpty (Point d r :+ p) -> RT i d v p r
createRangeTree = undefined
-- RangeTree . GRT.createTree
--                 . fmap (\p -> last (p^.core.vector) :+ Leaf [p])


--------------------------------------------------------------------------------

-- | Gets all points in the range tree
toAscList :: RT i d v p r -> [Point d r :+ p]
toAscList = concatMap (^.extra.to _getPts) . F.toList . GRT.toAscList . _unRangeTree


--------------------------------------------------------------------------------

createRangeTree1 :: (Ord r, RTMeasure v d p r, 1 <= d, Arity d)
                 => NonEmpty (Point d r :+ p) -> RT 1 d v p r
createRangeTree1 = RangeTree . GRT.createTree
                . fmap (\p -> head (p^.core.vector) :+ Leaf [p])

createRangeTree2 :: forall v d r p. (Ord r, RTMeasure v d p r, Arity d, 2 <= d
                                    , 1 <= d -- this one is kind of silly
                 ) => NonEmpty (Point d r :+ p) -> RT 2 d v p r
createRangeTree2 = RangeTree . GRT.createTree
                 . fmap (\p -> p^.core.coord (Proxy :: Proxy 2) :+ Leaf [p])

--------------------------------------------------------------------------------
-- * Querying


search   :: ( Ord r, Monoid (v (Point d r :+ p)), Query i d)
         => Vector d (Range r) -> RT i d v p r -> v (Point d r :+ p)
search r = mconcat . search' r


class (i <= d, Arity d) => Query i d where
  search' :: Ord r => Vector d (Range r) -> RT i d v p r -> [v (Point d r :+ p)]

instance (1 <= d, Arity d) => Query 1 d where
  search' qr = map unAssoc . GRT.search' r . _unRangeTree
    where
      r = qr^.element (Proxy :: Proxy 0)

instance ( 1 <= d, i <= d, Query (i-1) d, Arity d
         , i ~ 2
         ) => Query 2 d where
  search' qr = concatMap (maybe [] (search' qr) . unAssoc) . GRT.search' r . _unRangeTree
    where
      r = qr^.element (Proxy :: Proxy (i-1))
