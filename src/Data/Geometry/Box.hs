{-# LANGUAGE TemplateHaskell  #-}
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

import           Control.Lens(Getter,to,(^.))
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector(Arity)

import           Data.Ext

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

instance HasPoints (Box d p r) where
  points b = map (^.core) . catMaybes $ [b^.minPoint, b^.maxPoint]


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
----------------------------------------

type Rectangle = Box 2

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
