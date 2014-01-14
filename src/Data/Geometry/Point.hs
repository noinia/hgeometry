{-# LANGUAGE UndecidableInstances #-}
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
module Data.Geometry.Point where

-- import Control.Applicative
import Control.Lens

import Data.AdditiveGroup
import Data.AffineSpace

-- import Data.AdditiveGroup
-- import Data.VectorSpace

import Data.Geometry.Vector

import Data.Vinyl
import Data.Vinyl.Unicode

import GHC.TypeLits

--------------------------------------------------------------------------------

x :: "x" ::: r
x = Field :: "x" ::: r

y :: "y" ::: r
y = Field :: "y" ::: r

z :: "z" ::: r
z = Field :: "z" ::: z

type HasXY fields r = ( ("x" ::: r) ∈ fields
                      , ("y" ::: r) ∈ fields
                      )

data Point' (fields :: [*]) (r :: *) where
  Point' :: HasXY fields r => PlainRec fields -> Point' fields r


-- type family HasElem (x :: *) (xs :: [*]) (ys :: [*]) :: Constraint

-- type instance HasElem x xs '[] = IElem



-- instance (IElem x xs, ISubset xs ys) => Implicit (Elem x ys) where
--   implicitly = extract implicitly implicitly


-- extract :: Elem x xs -> Subset xs ys -> Elem x ys
-- extract _ (SubsetCons (elemProof :: Elem x ys) _) = elemProof
-- -- extract p (SubsetCons _ subsetProof) = There $ extract p subsetProof




-- (<++>) :: Point' p r -> PlainRec fs -> Point' (p ++ fs) r
-- (Point' r) <++> r' = Point' (r <+> r')

-- testz :: Point' p Int -> PlainRec pr -> Point' (pr ++ p) Int
-- testz (Point' r) r' = Point' $ r' <+> r



type family Dimension c :: Nat

type family NumType c :: *


class AsPlainRec c where
  type RecFields c :: [*]

  _rec :: Lens' c (PlainRec (RecFields c))


type family DimFields (d :: Nat) r :: [*]

type instance DimFields 0 r = '[]
type instance DimFields 1 r = '["x" ::: r]
type instance DimFields 2 r = ["x" ::: r, "y" ::: r]
type instance DimFields 3 r = ["x" ::: r, "y" ::: r, "z" ::: r]






data Point (d:: Nat) (r :: *) (fields :: [*]) where
  Point :: (DimFields d r ⊆ fields) => PlainRec fields -> Point d r fields

instance AsPlainRec (Point d r p) where
  type RecFields (Point d r p) = p
  _rec = undefined              -- TODO: Define a lens

type instance Dimension (Point d r p) = d
type instance NumType (Point d r p) = r



class HasVec p where
  asVec :: p -> Vec (Dimension p) (NumType p)


instance HasVec (Point 0 r p) where
  asVec _ = Vec []

-- instance HasVec (Point 1 r p) where
--   asVec p = Vec [p^._rec^._x]




instance (SingI (d :: Nat), Num r, HasVec (Point d r p)) =>
         AffineSpace (Point d r p) where
  type Diff (Point d r p) = Vec d r

  p .-. q = asVec p ^-^  asVec q

  p@(Point r) .+^ v = let xs = toList $ asVec p ^+^ v in
                      Point . updateFields r $ zip fields xs

toList = undefined

updateFields = undefined

fields = undefined



-- myPoint :: Point 2 Int ["x" ::: Int, "y" ::: Int]
-- myPoint = Point $ x =: 5 <+> y =: 6


--     HasXY fields r => PlainRec fields -> Point fields r























-- _x :: (("x" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _x = rLens x . asPlainRec

-- _y :: (("y" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _y = rLens y . asPlainRec

-- _z :: (("z" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _z = rLens z
