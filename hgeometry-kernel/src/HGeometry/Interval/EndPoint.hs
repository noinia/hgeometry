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
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Interval.EndPoint
  ( EndPoint_(..)
  , HasEndPoint(..)

  , EndPointType(..)
  , EndPoint(.., OpenE, ClosedE)
  , AnEndPoint(..)
  ) where

import Control.Lens
import HGeometry.Properties

--------------------------------------------------------------------------------

class HasEndPoint endPoint endPoint' where
  -- | Lens to access the actual data value of the end point
  endPoint :: Lens endPoint endPoint' (NumType endPoint) (NumType endPoint')

-- | An endpoint storing values of some type r
class HasEndPoint endPoint endPoint => EndPoint_ endPoint where
  -- | Report the type of the endpoint
  endPointType :: endPoint -> EndPointType
  -- | constructs a "default" enpoint
  mkEndPoint :: NumType endPoint -> endPoint

-- | Possible endpoint types; open or closed
data EndPointType = Open | Closed deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | EndPoint with a type safe tag
newtype EndPoint (et :: EndPointType) r = EndPoint r deriving (Show,Eq,Ord)

type instance NumType   (EndPoint et r) = r

instance HasEndPoint (EndPoint et r) (EndPoint et r') where
  endPoint = lens (\(EndPoint x) -> x) (\_ x -> EndPoint x)

instance EndPoint_ (EndPoint Closed r) where
  endPointType _ = Closed
  mkEndPoint = EndPoint

instance EndPoint_ (EndPoint Open r) where
  endPointType _ = Open
  mkEndPoint = EndPoint

pattern ClosedE   :: r -> EndPoint Closed r
pattern ClosedE x = EndPoint x

pattern OpenE   :: r -> EndPoint Open r
pattern OpenE x = EndPoint x


-- | Data type modelling an endpoint that can both be open and closed.
data AnEndPoint r = AnEndPoint !EndPointType !r
                  deriving (Show,Eq,Functor,Foldable,Traversable)

type instance NumType (AnEndPoint r) = r

instance HasEndPoint (AnEndPoint r) (AnEndPoint r') where
  endPoint = lens (\(AnEndPoint _ p) -> p) (\(AnEndPoint t _) p -> AnEndPoint t p)

instance EndPoint_ (AnEndPoint r) where
  endPointType (AnEndPoint t _) = t
  -- | By default we consider endpoints closed
  mkEndPoint = AnEndPoint Closed
