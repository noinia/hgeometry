{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Interval.EndPoint
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Endpoints of intervals
--
--------------------------------------------------------------------------------
module HGeometry.Interval.EndPoint
  ( EndPoint_(..)
  , IsEndPoint(..)

  , EndPointType(..)
  , EndPoint(EndPoint, OpenE, ClosedE)
  , AnEndPoint(..)
  ) where

import Control.Lens
import HGeometry.Properties
import HGeometry.Vector
import Data.Semigroup.Foldable.Class

import HGeometry.Point
--------------------------------------------------------------------------------

-- | Types that have an '_endPoint' field lens.
class IsEndPoint endPoint endPoint' where
  -- | Lens to access the actual data value of the end point
  _endPoint :: Lens endPoint endPoint' (IxValue endPoint) (IxValue endPoint')

-- | An endpoint storing values of some type r
class IsEndPoint endPoint endPoint => EndPoint_ endPoint where
  -- | Report the type of the endpoint
  endPointType :: endPoint -> EndPointType
  -- | constructs a "default" enpoint
  mkEndPoint :: IxValue endPoint -> endPoint

-- | Possible endpoint types; open or closed
data EndPointType = Open | Closed deriving (Show,Eq,Enum,Bounded)


testV :: Vector 2 (Point 2 Double)
testV = Vector2 (Point2 5.0 6.0) (Point2 10.0 1.0)

testV1 :: Vector 2 (EndPoint Closed (Point 2 Double))
testV1 = Vector2 (mkEndPoint$ Point2 5.0 6.0) (mkEndPoint$ Point2 10.0 1.0)

--------------------------------------------------------------------------------

-- | EndPoint with a type safe tag
newtype EndPoint (et :: EndPointType) r = EndPoint r
  deriving stock (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Foldable1 (EndPoint et)
instance Traversable1 (EndPoint et) where
  traverse1 f (EndPoint x) = EndPoint <$> f x


type instance NumType   (EndPoint et r) = r
type instance IxValue   (EndPoint et r) = r

type instance VectorFamily 2 (EndPoint et r) = WrapVector 2 r (EndPoint et r)


-- | Class for types that have '_endPoint' field.
instance IsEndPoint (EndPoint et r) (EndPoint et r') where
  _endPoint = lens (\(EndPoint x) -> x) (\_ x -> EndPoint x)

instance EndPoint_ (EndPoint Closed r) where
  endPointType _ = Closed
  mkEndPoint = EndPoint

instance EndPoint_ (EndPoint Open r) where
  endPointType _ = Open
  mkEndPoint = EndPoint

-- | Constructs a closed endpoint
pattern ClosedE   :: r -> EndPoint Closed r
pattern ClosedE x = EndPoint x

-- | Constructs an Open endpoint
pattern OpenE   :: r -> EndPoint Open r
pattern OpenE x = EndPoint x


-- | Data type modelling an endpoint that can both be open and closed.
data AnEndPoint r = AnEndPoint !EndPointType !r
                  deriving (Show,Eq,Functor,Foldable,Traversable)

type instance NumType (AnEndPoint r) = r
type instance IxValue (AnEndPoint r) = r

instance Foldable1 AnEndPoint
instance Traversable1 AnEndPoint where
  traverse1 f (AnEndPoint et x) = AnEndPoint et <$> f x


instance IsEndPoint (AnEndPoint r) (AnEndPoint r') where
  _endPoint = lens (\(AnEndPoint _ p) -> p) (\(AnEndPoint t _) p -> AnEndPoint t p)

instance EndPoint_ (AnEndPoint r) where
  endPointType (AnEndPoint t _) = t
  -- | By default we consider endpoints closed
  mkEndPoint = AnEndPoint Closed


--------------------------------------------------------------------------------
