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
import           Linear.Affine hiding (Point(..), origin)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = v3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------

-- | A d-dimensional point.
newtype Point d r = Point { toVec :: Vector d r }

-- | Lens to access the vector corresponding to this point.
--
-- >>> (point3 1 2 3) ^. vector
-- Vector {_unV = fromList [1,2,3]}
-- >>> origin & vector .~ v3 1 2 3
-- Point {toVec = Vector {_unV = fromList [1,2,3]}}
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

-- | Get the coordinate in a given dimension. This operation is unsafe in the
-- sense that no bounds are checked. Consider using `coord` instead.
--
--
-- >>> point3 1 2 3 ^. unsafeCoord 2
-- 2
unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
unsafeCoord i = vector . FV.element (i-1)
                -- Points are 1 indexed, vectors are 0 indexed

-- | Get the coordinate in a given dimension
--
-- >>> point3 1 2 3 ^. coord (C :: C 2)
-- 2
-- >>> point3 1 2 3 & coord (C :: C 1) .~ 10
-- Point {toVec = Vector {_unV = fromList [10,2,3]}}
-- >>> point3 1 2 3 & coord (C :: C 3) %~ (+1)
-- Point {toVec = Vector {_unV = fromList [1,2,4]}}
coord   :: forall i d r. (Index' (i-1) d, Arity d) => C i -> Lens' (Point d r) r
coord _ = vector . Vec.element (Proxy :: Proxy (i-1))


--------------------------------------------------------------------------------
-- | Convenience functions to construct 2 and 3 dimensional points


-- | We provide pattern synonyms Point2 and Point3 for 2 and 3 dimensional points. i.e.
-- we can write:
--
-- >>> :{
--   let
--     f              :: Point 2 r -> r
--     f (Point2 x y) = x
--   in f (point2 1 2)
-- :}
-- 1
--
-- if we want.
pattern Point2 x y   <- (_point2 -> (x,y))

-- | Similarly, we can write:
--
-- >>> :{
--   let
--     g                :: Point 3 r -> r
--     g (Point3 x y z) = z
--   in g myPoint
-- :}
-- 3
pattern Point3 x y z <- (_point3 -> (x,y,z))

-- | Construct a 2 dimensional point
--
-- >>> point2 1 2
-- Point {toVec = Vector {_unV = fromList [1,2]}}
point2     :: r -> r -> Point 2 r
point2 x y = Point $ v2 x y

-- | Destruct a 2 dimensional point
--
-- >>> _point2 $ point2 1 2
-- (1,2)
_point2   :: Point 2 r -> (r,r)
_point2 p = (p^.unsafeCoord 1, p^.unsafeCoord 2)


-- | Construct a 3 dimensional point
--
-- >>> point3 1 2 3
-- Point {toVec = Vector {_unV = fromList [1,2,3]}}
point3       :: r -> r -> r -> Point 3 r
point3 x y z = Point $ v3 x y z

-- | Destruct a 3 dimensional point
--
-- >>> _point3 $ point3 1 2 3
-- (1,2,3)
_point3   :: Point 3 r -> (r,r,r)
_point3 p = (p^.unsafeCoord 1, p^.unsafeCoord 2, p^.unsafeCoord 3)

--------------------------------------------------------------------------------

-- | A class for types that store points
class HasPoints g where
  points :: g -> [Point (Dimension g) (NumType g)]

class PointFunctor g where
  pmap :: (Point (Dimension (g r)) r -> Point (Dimension (g s)) s) -> g r -> g s

  -- pemap :: (d ~ Dimension (g r)) => (Point d r :+ p -> Point d s :+ p) -> g r -> g s
  -- pemap =

instance PointFunctor (Point d) where
  pmap f = f


-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point {toVec = Vector {_unV = fromList [0,0,0,0]}}
origin :: (Arity d, Num r) => Point d r
origin = Point $ pure 0
