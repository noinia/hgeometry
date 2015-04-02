{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box( Box(..) , Rectangle
                        , IsBoxable(..)
                        , IsAlwaysTrueBoundingBox
                        , minPoint, maxPoint
                        , boundingBoxList
                        ) where

import           Control.Applicative
import qualified Data.Foldable as F
import           Data.Maybe(catMaybes)
import           Data.Semigroup

import           Control.Lens(Getter,to,(^.),view)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.Geometry.Vector as V
import           Data.Geometry.Vector(Vector, Arity, Index',C(..))
import           Linear.Affine((.-.))

import           GHC.TypeLits
--------------------------------------------------------------------------------

data Box d p r = Empty
               | Box { _minP :: Min (Point d r) :+ p
                     , _maxP :: Max (Point d r) :+ p
                     }

deriving instance (Show r, Show p, Arity d) => Show (Box d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq   (Box d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord  (Box d p r)

instance (Arity d, Ord r, Semigroup p) => Semigroup (Box d p r) where
  Empty       <> b             = b
  b           <> Empty         = b
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')


instance (Arity d, Ord r, Semigroup p) => Monoid (Box d p r) where
  mempty = Empty
  b `mappend` b' = b <> b'


instance (Arity d, Ord r) => (Box d p r) `IsIntersectableWith` (Box d p r) where
  data Intersection (Box d p r) (Box d p r) = BoxBoxIntersection !(Box d () r)

  nonEmptyIntersection (BoxBoxIntersection Empty) = True
  nonEmptyIntersection _                          = False

  (Box a b) `intersect` (Box c d) = BoxBoxIntersection $ Box (mi :+ ()) (ma :+ ())
    where
      mi = (a^.core) `max` (c^.core)
      ma = (b^.core) `min` (d^.core)

deriving instance (Show r, Show p, Arity d) => Show (Intersection (Box d p r) (Box d p r))
deriving instance (Eq r, Eq p, Arity d)     => Eq   (Intersection (Box d p r) (Box d p r))
deriving instance (Ord r, Ord p, Arity d)   => Ord  (Intersection (Box d p r) (Box d p r))




-- Note that this does not guarantee the box is still a proper box
instance PointFunctor (Box d p) where
  pmap f (Box mi ma) = Box (fmap f <$> mi) (fmap f <$> ma)

instance (Num r, AlwaysTruePFT d) => IsTransformable (Box d p r) where
  transformBy = transformPointFunctor


type instance Dimension (Box d p r) = d
type instance NumType   (Box d p r) = r

to'     :: (m -> Point d r) -> (Box d p r -> m :+ p) ->
           Getter (Box d p r) (Maybe (Point d r :+ p))
to' f g = to $ \x -> case x of
                  Empty -> Nothing
                  b     -> Just . fmap f . g $ b

minPoint :: Getter (Box d p r) (Maybe (Point d r :+ p))
minPoint = to' getMin _minP

maxPoint :: Getter (Box d p r) (Maybe (Point d r :+ p))
maxPoint = to' getMax _maxP

-- | Get the size of the box (in all dimensions). Note that the resulting vector is 0 indexed
-- whereas one would normally count dimensions starting at zero.
--
-- >>> size (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- Vector {_unV = fromList [1,2,3]}
size                                 :: (Arity d, Num r) => Box d p r -> Vector d r
size Empty                           = pure 0
size (Box (Min a :+ _) (Max b :+ _)) = b .-. a

-- | Given a dimension, get the width of the box in that dimension. Dimensions are 1 indexed.
--
-- >>> widthIn (C :: C 1) (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- 1
-- >>> widthIn (C :: C 3) (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- 3
widthIn   :: forall proxy p i d r. (Arity d, Num r, Index' (i-1) d) => proxy i -> Box d p r -> r
widthIn _ = view (V.element (C :: C (i - 1))) . size

----------------------------------------

type Rectangle = Box 2

-- >>> width (boundingBoxList [origin, point2 1 2] :: Rectangle () Int)
-- 1
-- >>> width (boundingBoxList [origin] :: Rectangle () Int)
-- 0
width :: Num r => Rectangle p r -> r
width = widthIn (C :: C 1)

-- >>> height (boundingBoxList [origin, point2 1 2] :: Rectangle () Int)
-- 2
-- >>> height (boundingBoxList [origin] :: Rectangle () Int)
-- 0
height :: Num r => Rectangle p r -> r
height = widthIn (C :: C 2)


--------------------------------------------------------------------------------

class IsBoxable g where
  boundingBox :: (Monoid p, Semigroup p, Ord (NumType g))
              => g -> Box (Dimension g) p (NumType g)

type IsAlwaysTrueBoundingBox g p = (Semigroup p, Arity (Dimension g))


boundingBoxList :: (IsBoxable g, Monoid p, F.Foldable c, Ord (NumType g)
                   , IsAlwaysTrueBoundingBox g p
                   ) => c g -> Box (Dimension g) p (NumType g)
boundingBoxList = F.foldMap boundingBox

----------------------------------------

instance IsBoxable (Point d r) where
  boundingBox p = Box (Min p :+ mempty) (Max p :+ mempty)
