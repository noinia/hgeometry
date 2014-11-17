{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box( Box(..) , Rectangle
                        , IsBoxable(..)
                        , IsAlwaysTrueBoundingBox
                        , minPoint, maxPoint
                        , boundingBoxList
                        ) where


import qualified Data.Foldable as F
import Data.Semigroup
import Data.List.NonEmpty

import Control.Lens(Getter,to)
import Data.Geometry.Point
import Data.Geometry.Properties
import           Data.Geometry.Vector(Arity)

import Data.Ext

--------------------------------------------------------------------------------

data Box d pe r = Empty
                | Box { _minP :: Min (Point d r) :+ pe
                      , _maxP :: Max (Point d r) :+ pe
                      }

deriving instance (Show r, Show pe, Arity d) => Show (Box d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq   (Box d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord  (Box d pe r)

instance (Arity d, Ord r, Semigroup pe) => Semigroup (Box d pe r) where
  Empty       <> b             = b
  b           <> Empty         = b
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')


instance (Arity d, Ord r, Semigroup pe) => Monoid (Box d pe r) where
  mempty = Empty
  b `mappend` b' = b <> b'

type instance Dimension (Box d pe r) = d
type instance NumType   (Box d pe r) = r

to'     :: (m -> Point d r) -> (Box d pe r -> m :+ pe) ->
           Getter (Box d pe r) (Maybe (Point d r :+ pe))
to' f g = to $ \x -> case x of
                  Empty -> Nothing
                  b     -> Just . fmap f . g $ b

minPoint :: Getter (Box d pe r) (Maybe (Point d r :+ pe))
minPoint = to' getMin _minP

maxPoint :: Getter (Box d pe r) (Maybe (Point d r :+ pe))
maxPoint = to' getMax _maxP
----------------------------------------

type Rectangle = Box 2

--------------------------------------------------------------------------------

class IsBoxable g where
  boundingBox :: (Monoid pe, Semigroup pe, Ord (NumType g))
              => g -> Box (Dimension g) pe (NumType g)

type IsAlwaysTrueBoundingBox g pe = (Semigroup pe, Arity (Dimension g))


boundingBoxList :: (IsBoxable g, Monoid pe, F.Foldable c, Ord (NumType g)
                   , IsAlwaysTrueBoundingBox g pe
                   ) => c g -> Box (Dimension g) pe (NumType g)
boundingBoxList = F.foldMap boundingBox

----------------------------------------

instance IsBoxable (Point d r) where
  boundingBox p = Box (Min p :+ mempty) (Max p :+ mempty)
