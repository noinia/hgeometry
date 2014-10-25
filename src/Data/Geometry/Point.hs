{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Point where

import           Data.Proxy

import           Control.Applicative
import           Control.Lens

-- import Data.TypeLevel.Common
import           Data.Typeable
import           Data.Vinyl hiding (Nat)
import qualified Data.Vinyl as V
import           Data.Vinyl.Functor(Const(..))
import qualified Data.Vinyl.TypeLevel as TV

import           Data.Vinyl.TypeLevel hiding (Nat)

import qualified Data.Vector.Fixed as FV
-- import qualified Data.Vector.Fixed.Cont as C


import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           GHC.TypeLits

import qualified Data.Geometry.Vector as Vec

import           Linear.Vector(Additive(..))
import           Linear.Affine hiding (Point(..))

--------------------------------------------------------------------------------

newtype Point d r = Point { toVec :: Vector d r }

vector :: Lens' (Point d r) (Vector d r)
vector = lens toVec (const Point)

deriving instance (Show r, Arity d) => Show (Point d r)
deriving instance (Eq r, Arity d)   => Eq (Point d r)
deriving instance (Ord r, Arity d)  => Ord (Point d r)
deriving instance Arity d           => Functor (Point d)


type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d



instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v

unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
unsafeCoord i = vector . FV.element (i-1)
                -- Points are 1 indexed, vectors are 0 indexed

coord   :: forall i d r. (Index' (i-1) d, Arity d) => C i -> Lens' (Point d r) r
coord _ = vector . Vec.element (Proxy :: Proxy (i-1))


pattern Point2 x y <- (_point2 -> (x,y))

point2     :: r -> r -> Point 2 r
point2 x y = Point . Vector $ FV.mk2 x y

_point2   :: Point 2 r -> (r,r)
_point2 p = (p ^. unsafeCoord 1, p ^. unsafeCoord 2)


class HasPoints t where
  points :: t -> [Point (Dimension t) (NumType t)]


origin :: (Arity d, Num r) => Point d r
origin = Point $ pure 0
