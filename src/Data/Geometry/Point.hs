{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TemplateHaskell #-}
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
import Data.Vinyl.Universe.Field


import GHC.TypeLits


--------------------------------------------------------------------------------
-- data MField :: * -> * where
--   Dim :: KnownNat n => Proxy n -> MField (n ::: t)

data Dim (n :: Nat) where
  Dim :: KnownNat n => Dim n









data DimElField :: * -> (TyFun * *) -> * where
  DimElField :: DimElField t el

type instance App (DimElField r) (sy ::: t) = t
type instance App (DimElField r) (Dim n)    = r

data MField :: * -> * where
  MField :: KnownNat n => Dim n -> MField (Dim n)








data DField :: * -> * where
  DField :: KnownNat n => Dim n -> DField ((Dim n) ::: t)

myPoint :: PlainRec ElField [Dim 1 ::: Int,"name" ::: String]
myPoint = DField (Dim :: Dim 1) =: 5
           <+>
           SField =: "Frank"


-- -- | Make a
-- data DField :: * -> * where
--   DField :: KnownNat n => DField (n ::: t)

myPoint2 :: PlainRec (DimElField Int) [Dim 1,Dim 2]
myPoint2 = MField (Dim :: Dim 1)  =: 5
           <+>
           MField (Dim :: Dim 2) =: 10
           -- SField =: "Frank"

-- | Make a

-- type D (n :: Nat) = DField (Dim n)

-- dim :: forall n. KnownNat n => DField (Dim n)
-- dim = DField (Dim :: Dim n)





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
