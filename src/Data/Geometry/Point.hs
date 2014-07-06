{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE PolyKinds #-}     --- TODO: Why do we need this?

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Geometry.Point( --Point(..)
                          -- , point

                          ) where

-- import Control.Applicative
import Control.Lens


import Linear.Affine hiding (Point(..))
import Linear.Vector

import Data.Maybe

import Data.Geometry.Properties
import Data.Geometry.Vector

-- import Data.Singletons.TH

import Data.Vinyl
import Data.Vinyl.TyFun
import Data.Vinyl.TH
import Data.Vinyl.Universe.Field((:::), ElField(..))


import GHC.TypeLits

-- | A type expressing a dimension. Note that something like 'D 2' expresses
-- *only* the second dimention (in whatever space we consider). Say the
-- two-dimensional plane is expressed by the type `R 2`.
data D (d :: Nat)
  -- essentially, we needed a wrapper type to get back from kind nat to kind *


-- | Fields are indexed by either a type level natural number (a dimension), or
-- by a type level string. If it is a natural number, the type of the value
-- will be determined by the interpretation function. If it is a string, we can
-- specify the type ourselves.
data Field :: * -> * where
  NatField :: KnownNat d    => Field (D d)
  SymField :: KnownSymbol s => Field (s ::: t)

-- | Singletons for Field
data SField :: * -> * where
  SNatField :: KnownNat d    => SField (Field (D d))
  SSymField :: KnownSymbol s => SField (Field (s ::: t))


-- | The universe/interpretation of the fields. The universe is paramteterized
  -- over a type t and a TyFun. Th type t is used as a ``default'' type, for
  -- fields for which we did not specify the type.
data TElField :: * -> (TyFun * *) -> * where
  TElField :: TElField t el

type instance App (TElField r) (Field (D d))     = r
type instance App (TElField r) (Field (s ::: t)) = t


-- | Shorthand for naming dimention fields
type DField (d :: Nat)  = Field (D d)

-- | And a fancy name for the symbol fields
type (s :: Symbol) :~>: (t :: *) = Field (s ::: t)


-- | Similar shorthands for the corresponding singletons
type SDField (d :: Nat)             = SField (DField d)
type SSField (s :: Symbol) (t :: *) = SField (s :~>: t)






data R (d :: Nat)




x :: SDField 1
x = SNatField


name :: SSField "name" String --SField (Field ("name" ::: String))
name = SSymField





-- data Point (n :: Nat) (tElF :: * -> *) (fields :: [*]) where
--   Point :: PlainRec tElF fields



pt :: PlainRec (TElField Int) [DField 1, "name" :~>: String]
pt =   x    =: 10
   <+> name =: "frank"




--------------------------------------------------------------------------------
-- | The data type for a point.

-- | A Point in a d dimensional space. Apart from coordinates in R^d may have
-- additonal fields/attributes. For example color, a label, etc. The data type
-- is intentionally left abstract. To construct a point, use fromVector,
-- origin, or `point`.




-- data Point (d:: Nat) (r :: *) (fields :: [*]) where
--   Point :: Vec d r -> PlainRec fields -> Point d r fields

-- type instance Dimension (Point d r p) = d
-- type instance NumType (Point d r p) = r

-- -- instance Num r => AffineSpace (Point d r p) where
-- --   type Diff (Point d r p) = Vec d r

-- --   p .-. q = asVec p ^-^  asVec q

-- --   (Point u fs) .+^ v = Point (u ^+^ v) fs


-- asVec             :: Point d r p -> Vec d r
-- asVec (Point v _) = v


--------------------------------------------------------------------------------
-- | Constructing Points

-- fromVector :: Vec d r -> Point d r '[]
-- fromVector = flip Point RNil

-- origin :: Num r => Point d r '[]
-- origin = fromVector zeroV
