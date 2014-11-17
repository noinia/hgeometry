{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box where


import qualified Data.Foldable as F
import Data.Semigroup
import Data.List.NonEmpty

import Control.Lens(Getter,to)
import Data.Geometry.Point
import Data.Geometry.Properties
import           Data.Geometry.Vector(Arity)


import Data.Ext

--------------------------------------------------------------------------------

data Box d pe r = Box { _minP :: Min (Point d r) :+ pe
                      , _maxP :: Max (Point d r) :+ pe
                      }

deriving instance (Show r, Show pe, Arity d) => Show (Box d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq   (Box d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord  (Box d pe r)

instance (Arity d, Ord r, Semigroup pe) => Semigroup (Box d pe r) where
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')

type instance Dimension (Box d pe r) = d
type instance NumType   (Box d pe r) = r


minPoint :: Getter (Box d pe r) ((Point d r) :+ pe)
minPoint = to (fmap getMin . _minP)

maxPoint :: Getter (Box d pe r) ((Point d r) :+ pe)
maxPoint = to (fmap getMax . _maxP)

----------------------------------------

type Rectangle = Box 2

--------------------------------------------------------------------------------

class IsBoxable g where
  boundingBox :: (Monoid pe, Ord (NumType g)) => g -> Box (Dimension g) pe (NumType g)

type IsAlwaysTrueBoundingBox g pe = (Semigroup pe, Arity (Dimension g))


boundingBox' :: ( IsBoxable g, Ord (NumType g), Monoid pe
                , IsAlwaysTrueBoundingBox g pe
                ) => NonEmpty g -> Box (Dimension g) pe (NumType g)
boundingBox' = sconcat . fmap boundingBox


instance IsBoxable (Point d r) where
  boundingBox p = Box (Min p :+ mempty) (Max p :+ mempty)
