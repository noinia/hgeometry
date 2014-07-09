{-# LANGUAGE UndecidableInstances #-}    -- Def R1


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-} --  lens stuff


{-# LANGUAGE PolyKinds #-}     --- TODO: Why do we need this?




module Data.Geometry.Point( --Point(..)
                          -- , point

                          ) where

-- import Control.Applicative
import Control.Lens(Lens')




import Linear.Affine hiding (Point(..))
import Linear.Vector

import Data.Maybe

import Data.Geometry.Properties
import Data.Geometry.Vector

-- import Data.Singletons.TH

import Data.Vinyl
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.TyFun
import Data.Vinyl.Lens
import Data.Vinyl.Universe.Geometry


import Data.Type.Nat

import GHC.TypeLits

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | A defintition of a d dimentional space

-- | R d is a type level list containing all DFields for dimensions 1 t/m d
type R (d :: Nat) = R1 (ToNat1 d)

-- | The implementation of R uses the Peano nats
type family R1 (d :: Nat1) :: [*] where
  R1 Zero     = '[]
  R1 (Succ n) = R1 n ++ '[DField (FromNat1 (Succ n))]

-- | Type level list concatenation
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++

--------------------------------------------------------------------------------
-- | Defining points in a d-dimensional space

-- | A Point in a d dimensional space. Apart from coordinates in R^d may have
-- additonal fields/attributes. For example color, a label, etc.
data Point (d :: Nat) (r :: *) (fields :: [*]) where
  -- Point :: (fields <: R d) => PlainRec (TElField r) fields -> Point d r fields
  Point :: ( allFields ~ (R d ++ fields)) =>
           PlainRec (TElField r) allFields -> Point d r fields

-- | Smart constructor that allows a different order of the input fields
point :: ( allFields' ~ (R d ++ fields)
         , allFields :~: allFields'
         ) => PlainRec (TElField r) allFields -> Point d r fields
point = Point . cast


-- | Some hands for the axis (fields) in the first three dimensions
x = SNatField :: SDField 1
y = SNatField :: SDField 2
z = SNatField :: SDField 3

-- | And a regular named field
name :: SSField "name" String --SField (Field ("name" ::: String))
name = SSymField


----------------------------------------
 -- bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)

_rec             :: (allFields ~ (R d ++ fields))
                 => Lens' (Point d r fields) (PlainRec (TElField r) allFields)
_rec f (Point r) = fmap (\r' -> Point r') (f r)


-- _xL :: (DField 1 ∈ R1 d) => Lens' (PlainRec (TElField r) (R1 d)) r
-- _xL = rLens x

-- _x :: (DField 1 ∈ R d) => Lens' (Point d r fields) r
-- _x = _rec._xL


--          Point r -> _xL r
-- _x   :: (DField 1 ∈ R d) => Point d r fields -> Identity r
-- -- _x   :: (1 <= d) => Point d r fields -> Identity r
-- _x p = case p of
--          Point (r :: PlainRec (TElField r) (R d ++ fields)) -> rGet' x r



pt :: PlainRec (TElField Int) [DField 1, "name" :~>: String]
pt =   x    =: 10
   <+> name =: "frank"


pt2 = pt <+> y =: 5

myPt2 :: Point 2 Int '["name" :~>: String]
myPt2 = point pt2


myPt :: Point 1 Int '["name" :~>: String]
myPt = Point pt


myX :: Int
myX = case myPt of
        Point pt' -> runIdentity $ rGet' x pt'




myX1 :: Int
myX1 = runIdentity $ rGet' x pt



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
