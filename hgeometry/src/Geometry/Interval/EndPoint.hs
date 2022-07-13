--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Interval.EndPoint
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Endpoints of intervals
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Geometry.Interval.EndPoint
  ( EndPoint_(..)

  , EndPointType(..)
  , EndPoint(..)
  , Open(..)
  , Closed(..)
  ) where

import Control.Lens

--------------------------------------------------------------------------------

-- | An endpoint storing values of some type r
class Traversable endPoint => EndPoint_ endPoint where
  -- | Lens to access the actual data value of the end point
  endPoint     :: Lens (endPoint r) (endPoint r') r r'
  -- | Report the type of the endpoint
  endPointType :: endPoint r -> EndPointType
  -- | constructs a "default" enpoint
  mkEndPoint :: r -> endPoint r

-- | Possible endpoint types; open or closed
data EndPointType = OpenEndPoint | ClosedEndPoint deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | Data type modelling an endpoint that can both be open and closed.
data EndPoint r = EndPoint !EndPointType !r
                deriving (Show,Eq,Functor,Foldable,Traversable)

instance EndPoint_ EndPoint where
  endPoint     =
    lens (\(EndPoint _ p) -> p) (\(EndPoint t _) p -> EndPoint t p)
  endPointType (EndPoint t _) = t
  -- | By default we consider endpoints closed
  mkEndPoint = EndPoint ClosedEndPoint

--------------------------------------------------------------------------------

-- | Closed endpoints
newtype Closed r = Closed r deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance EndPoint_ Closed where
  endPoint = lens (\(Closed x) -> x) (\_ x -> Closed x)
  endPointType = const ClosedEndPoint
  mkEndPoint = Closed

--------------------------------------------------------------------------------

-- | Open endpoints
newtype Open r = Open r deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance EndPoint_ Open where
  endPoint = lens (\(Open x) -> x) (\_ x -> Open x)
  endPointType = const OpenEndPoint
  mkEndPoint = Open
