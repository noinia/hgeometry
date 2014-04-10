{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.Point( Point
                          , point

                          ) where

-- import Control.Applicative
import Control.Lens



import Data.AdditiveGroup
import Data.AffineSpace

import Data.Maybe

import Data.HList.HList
import Data.HList.HZip

-- import Data.AdditiveGroup
-- import Data.VectorSpace

import Data.Geometry.Properties
import Data.Geometry.Vector

import Data.Vinyl
import Data.Vinyl.Field
import Data.Vinyl.Relation
import Data.Vinyl.Unicode

import GHC.TypeLits

--------------------------------------------------------------------------------
-- | The data type for a point.

-- | A Point in a d dimensional space. Apart from coordinates in R^d may have
-- additonal fields/attributes. For example color, a label, etc. The data type
-- is intentionally left abstract. To construct a point, use fromVector,
-- origin, or `point`.
data Point (d:: Nat) (r :: *) (fields :: [*]) where
  Point :: Vec d r -> PlainRec fields -> Point d r fields

type instance Dimension (Point d r p) = d
type instance NumType (Point d r p) = r

instance Num r => AffineSpace (Point d r p) where
  type Diff (Point d r p) = Vec d r

  p .-. q = asVec p ^-^  asVec q

  (Point u fs) .+^ v = Point (u ^+^ v) fs




asVec             :: Point d r p -> Vec d r
asVec (Point v _) = v


--------------------------------------------------------------------------------
-- | Constructing Points

fromVector :: Vec d r -> Point d r '[]
fromVector = flip Point RNil

origin :: Num r => Point d r '[]
origin = fromVector zeroV



--------------------------------------------------------------------------------

point :: PointFrom d r fields => PlainRec fields -> Point d r (RemainingFields d r fields)
point = uncurry Point . splitPlainRec


class PointFrom (d :: Nat) (r :: *) (fields :: [*]) where
  type RemainingFields d r fields

  splitPlainRec :: PlainRec fields -> (Vec d r, PlainRec (RemainingFields d r fields))



prepare   :: (Rec fields f :~: Rec (dimFields ++ rest) f)
          => Rec fields f -> Rec (dimFields ++ rest) f
prepare r = cast (fixRecord r)






-- type family Remove (xs :: [*]) (x :: *) :: [*]
-- type instance Remove '[] x = '[]
-- type instance Remove (x ': xs) x = xs
-- type instance Remove (y ': xs) x = y ': Remove xs x



-- type family SetMinus (xs :: [*]) (ys :: [*]) :: [*]
-- type instance SetMinus xs '[]       = xs
-- type instance SetMinus xs (y ': ys) = SetMinus (Remove xs y) ys









x :: "x" ::: r
x = Field :: "x" ::: r

y :: "y" ::: r
y = Field :: "y" ::: r

z :: "z" ::: r
z = Field :: "z" ::: z

-- | type level snoc for lists of symbols
type family (ss :: [Symbol]) |> (s :: Symbol) :: [Symbol]
type instance '[]       |> s = '[s]
type instance (x ': ss) |> s = x ': (ss |> s)

-- | A dimension d has a list of names (type level strings) that name the axes
--   in a vector space of dimension d.
type family DimAxes (d :: Nat) :: [Symbol]

type instance DimAxes 0 = '[]
type instance DimAxes 1 = DimAxes 0 |> "x"
type instance DimAxes 2 = DimAxes 1 |> "y"
type instance DimAxes 3 = DimAxes 2 |> "z"

-- | Given a list of symbols (a type level string) and a type t. Create a
-- corresponding list of types, each of ``kind'' (::: t). I.e. creeate a list
-- of types corresponding to the Vinyl field names.
type family   ZipToField (xs :: [Symbol]) (t :: *) :: [*]

type instance ZipToField '[]        t = '[]
type instance ZipToField (sy ': ss) t = (sy ::: t) ': ZipToField ss t

-- -- | A dimension is associated with a number of fields.
type DimFields (d :: Nat) r = ZipToField (DimAxes d) r


-- type DimFields 1 r = ["x" ::: r]



class AsVec (t :: *) where
  toVec :: t -> Vec (Dimension t) (NumType t)

instance AsVec (Point 0 r p) where
  toVec _ = Vec []

-- instance AsVec (Point 1 r p) where
--   toVec (Point p) = Vec [rGet x p]

-- instance AsVec (Point 2 r p) where
--   toVec (Point p) = Vec Vec [p `rGet` x, p `rGet` y]
