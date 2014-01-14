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
module Data.Geometry.Point where

-- import Control.Applicative
import Control.Lens

-- import Data.AdditiveGroup
-- import Data.VectorSpace

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





-- instance Implicit (Elem x xs) => Implicit (Elem x (xs ++ '[])) where
--   implicitly = undefined




(<++>) :: (HasXY (p ++ fs) r) => Point' p r -> PlainRec fs -> Point' (p ++ fs) r
(Point' r) <++> r' = Point' (r <+> r')

-- testz :: Point' p Int -> PlainRec pr -> Point' (pr ++ p) Int
-- testz (Point' r) r' = Point' $ r' <+> r



type family Dimension c :: Nat

type family NumType c :: *


class AsPlainRec c where
  type RecFields c :: [*]

  _rec :: Lens' c (PlainRec (RecFields c))


type family DimFields (d :: Nat) r :: [*]

type instance DimFields 2 r = ["x" ::: r, "y" ::: r]
type instance DimFields 3 r = ["x" ::: r, "y" ::: r, "z" ::: r]






data Point (d:: Nat) (r :: *) (fields :: [*]) where
  Point :: (DimFields d r ⊆ fields) => PlainRec fields -> Point d r fields

instance AsPlainRec (Point d r p) where
  type RecFields (Point d r p) = p
  _rec = undefined

type instance Dimension (Point d r p) = d
type instance NumType (Point d r p) = r






myPoint :: Point 2 Int ["x" ::: Int, "y" ::: Int]
myPoint = Point $ x =: 5 <+> y =: 6


--     HasXY fields r => PlainRec fields -> Point fields r























-- _x :: (("x" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _x = rLens x . asPlainRec

-- _y :: (("y" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _y = rLens y . asPlainRec

-- _z :: (("z" ::: r) ∈ fields) => Lens' (PlainRec fields) r
-- _z = rLens z


-- type Point2 r fields = ( ("x" ::: r) ∈ fields
--                        , ("y" ::: r) ∈ fields
--                        )

-- type Point3 r fields = ( ("x" ::: r) ∈ fields
--                        , ("y" ::: r) ∈ fields
--                        , ("z" ::: r) ∈ fields
--                        )


-- p2         :: (r,r) -> PlainRec ["x" ::: r, "y" ::: r]
-- p2 (x',y') = x =: x' <+> y =: y'

-- unP2   :: Point2 r fields => PlainRec fields -> (r,r)
-- unP2 v = (v^._x, v^._y)

-- p3            :: (r,r,r) -> PlainRec ["x" ::: r, "y" ::: r, "z" ::: r]
-- p3 (x',y',z') = x =: x' <+> y =: y' <+> z =: z'

-- unP3   :: Point3 r fields => PlainRec fields -> (r,r,r)
-- unP3 v = (v^._x, v^._y,v^._z)












-- val :: PlainRec ["x" ::: Int, "y" ::: Int]
-- val = x =: 5 <+> y =: 6



-- foo = 6
