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
  , AnEndPoint(..,AnOpenE,AnClosedE)
  , asAnEndPoint
  ) where

import Control.Lens
import Data.Foldable1
import GHC.Generics (Generic)
import Control.DeepSeq
import HGeometry.Properties
import Text.Read

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
data EndPointType = Open | Closed deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData EndPointType

-- testV :: Vector 2 (Point 2 Double)
-- testV = Vector2 (Point2 5.0 6.0) (Point2 10.0 1.0)

-- testV1 :: Vector 2 (EndPoint Closed (Point 2 Double))
-- testV1 = Vector2 (mkEndPoint$ Point2 5.0 6.0) (mkEndPoint$ Point2 10.0 1.0)

--------------------------------------------------------------------------------

-- | EndPoint with a type safe tag
newtype EndPoint (et :: EndPointType) r = EndPoint r
  deriving stock (Eq,Ord,Functor,Foldable,Traversable,Generic)
  deriving newtype (NFData)

instance Show r => Show (EndPoint Closed r) where
  showsPrec = showsPrecImpl "ClosedE"
instance Show r => Show (EndPoint Open r) where
  showsPrec = showsPrecImpl "OpenE"

-- | Implementation for Show
showsPrecImpl                        :: Show r => String -> Int -> EndPoint et r -> ShowS
showsPrecImpl ctrName k (EndPoint x) = showParen (k > app_prec) $
                                           showString ctrName
                                         . showChar ' '
                                         . showsPrec (app_prec+1) x

-- | application precedence
app_prec :: Int
app_prec = 10

instance Read r => Read (EndPoint Closed r) where
  readPrec = parens $ (prec app_prec $ do
                          Ident "ClosedE" <- lexP
                          p <- step readPrec
                          return (ClosedE p))

instance Read r => Read (EndPoint Open r) where
  readPrec = parens $ (prec app_prec $ do
                          Ident "OpenE" <- lexP
                          p <- step readPrec
                          return (OpenE p))

instance Foldable1 (EndPoint et) where
  foldMap1 f (EndPoint x) = f x
instance Traversable1 (EndPoint et) where
  traverse1 f (EndPoint x) = EndPoint <$> f x


type instance NumType   (EndPoint et r) = r
type instance IxValue   (EndPoint et r) = r


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
{-# COMPLETE ClosedE #-}

-- | Constructs an Open endpoint
pattern OpenE   :: r -> EndPoint Open r
pattern OpenE x = EndPoint x
{-# COMPLETE OpenE #-}

-- | Data type modelling an endpoint that can both be open and closed.
data AnEndPoint r = AnEndPoint {-# UNPACK #-} !EndPointType !r
                  deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Generic)

type instance NumType (AnEndPoint r) = r
type instance IxValue (AnEndPoint r) = r


-- | Constructs a closed endpoint
pattern AnClosedE   :: r -> AnEndPoint r
pattern AnClosedE x = AnEndPoint Closed x

-- | Constructs an Open endpoint
pattern AnOpenE   :: r -> AnEndPoint r
pattern AnOpenE x = AnEndPoint Open x

-- type instance VectorFamily d (AnEndPoint r) = Boxed.Vector d (AnEndPoint r)

instance Foldable1 AnEndPoint where
  foldMap1 f (AnEndPoint _ x) = f x
instance Traversable1 AnEndPoint where
  traverse1 f (AnEndPoint et x) = AnEndPoint et <$> f x


instance IsEndPoint (AnEndPoint r) (AnEndPoint r') where
  _endPoint = lens (\(AnEndPoint _ p) -> p) (\(AnEndPoint t _) p -> AnEndPoint t p)

instance EndPoint_ (AnEndPoint r) where
  endPointType (AnEndPoint t _) = t
  -- | By default we consider endpoints closed
  mkEndPoint = AnEndPoint Closed

-- | Convert the endpoint into a, AnEndPoint
asAnEndPoint   :: EndPoint_ endPoint => endPoint -> AnEndPoint (IxValue endPoint)
asAnEndPoint p = case endPointType p of
                   Closed -> AnClosedE (p^._endPoint)
                   Open   -> AnOpenE   (p^._endPoint)

--------------------------------------------------------------------------------
