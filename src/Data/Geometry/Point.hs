{-# LANGUAGE UndecidableInstances #-}    -- Def R1

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-} --  lens stuff


{-# LANGUAGE PolyKinds #-}     --- TODO: Why do we need this?

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Geometry.Point( --Point(..)
                          -- , point

                          ) where

-- import Control.Applicative
import Control.Lens(Lens')




import Linear.Affine hiding (Point(..))
import Linear.Vector

import Data.Maybe
import Data.Proxy

import Data.Geometry.Properties
-- import Data.Geometry.Vector


import Data.Vector.Fixed.Boxed
import Data.Vector.Fixed.Cont
import Data.Vector.Fixed

import Data.Vinyl
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.TyFun
import Data.Vinyl.Lens
import Data.Vinyl.Universe.Geometry


import Data.Type.Nat

import GHC.TypeLits

import qualified Data.Vector.Fixed as V

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | A defintition of a d dimentional space

-- | R d is a type level list containing all DFields for dimensions 1 t/m d
-- type R (d :: Nat) = R1 (ToNat1 d)
type R (d :: Nat) = Range 1 d

-- -- | The implementation of R uses the Peano nats
-- type family R1 (d :: Nat1) :: [*] where
--   R1 Zero     = '[]
--   R1 (Succ n) = R1 n ++ '[DField (FromNat1 (Succ n))]

-- | Type level list concatenation
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++


type Range (s :: Nat) (k :: Nat) = Range1 (ToNat1 s) (ToNat1 k)


type family Range1 (s :: Nat1) (k :: Nat1) where
  Range1 s Zero     = '[]
  Range1 s (Succ k) = DField (FromNat1 s) ': Range1 (Succ s) k


-- type family Dropped (k :: Nat1) (xs :: [*]) where
--   Dropped Zero     xs        = xs
--   Dropped k        '[]       = '[]
--   Dropped (Succ k) (x ': xs) = Dropped k xs

-- class Drop (k :: Nat1) where
--   dropRec :: Proxy k -> Rec el f fields -> Rec el f (Dropped k fields)

-- instance Drop Zero where
--   dropRec = const id

-- instance Drop k => Drop (Succ k) where
--   dropRec _ RNil     = RNil
--   dropRec _ (_ :& r) = dropRec (Proxy :: Proxy k) r






-- -- | Dunno what to do about the weird Arity check
-- toVec   :: Arity (ToPeano d) => Proxy d -> PlainTRec r (R d) -> Vec (ToPeano d) r
-- toVec p = vector . toContVec p

type family Len (xs :: [*]) where
  Len '[]       = Z
  Len (x ': xs) = S (Len xs)

class ToContVec (rs :: [*]) where
  toContVec :: PlainTRec r rs -> ContVec (Len rs) r

instance ToContVec '[] where
  toContVec _ = empty

instance ToContVec rs => ToContVec (DField i ': rs) where
  toContVec (r :& rs) = runIdentity r <| toContVec rs







toVec              :: forall d r fs. ( Len (R d) ~ ToPeano d
                                     , ToContVec (R d)
                                     , Arity (ToPeano d)
                                     ) =>
                      Point' d r fs -> Vec (ToPeano d) r
toVec (Point' g _) = vector $ toContVec g




class VecField (i :: *) where
  vField :: SDField (ToNat (S i))

  vIndex   :: (Arity d, Index i d) => Vec d r -> r
  vIndex v = V.index v (undefined :: i)

instance VecField Z where
  vField = SNatField

instance KnownNat (ToNat (S (S (n)))) => VecField (S n) where
  vField = SNatField






type family Replicate (n :: *) (t :: *) where
  Replicate Z     t = '[]
  Replicate (S n) t = (t ': Replicate n t)

type family (n :: *) :-: (m :: *) where
  Z :-: m = Z
  n :-: Z = Z
  (S n) :-: (S m) = n :-: m

infixl 6 :-:



type Replicate1 (n :: Nat1) (t :: *) = Replicate (Nat1ToPeano n) t



type family PeanoToNat1 (n :: *) :: Nat1 where
  PeanoToNat1 Z = Zero
  PeanoToNat1 (S n) = Succ (PeanoToNat1 n)

type family Nat1ToPeano (n :: Nat1) :: * where
  Nat1ToPeano Zero     = Z
  Nat1ToPeano (Succ n) = S (Nat1ToPeano n)











class VecToHList (d :: *) where
  vecToHList :: Arity d => Vec d r -> HList (Replicate d r)

instance VecToHList Z where
  vecToHList _ = RNil

instance (Arity d, VecToHList d) => VecToHList (S d) where
  vecToHList v = let (x,xs) = (V.head v, V.tail v)
                 in (Identity x) :& vecToHList xs






class HListToPlainTRec (d :: Nat1) where
  hListToRec :: Proxy s -> Proxy d -> HList (Replicate1 d r) -> PlainTRec r (Range1 s d)


  -- dropOne :: PlainTRec r (R (ToNat d)) -> PlainTRec r (
  --                                           Dropped (S Z) (R (ToNat d)))

instance HListToPlainTRec Zero where
  hListToRec _ _ RNil = RNil


instance HListToPlainTRec d => HListToPlainTRec (Succ d) where
  hListToRec (ps :: Proxy s) _ (Identity r :& rs) = (Identity r)
                                                    :&
                                                    hListToRec (Proxy :: Proxy (Succ s)) (Proxy :: Proxy d) rs





vecToRec :: forall pd d r. (pd ~ ToPeano d, Arity (ToPeano d)) =>
            Proxy d ->
            Vec pd r -> PlainTRec r (R d)
vecToRec _ = hListToRec pOne pD . vecToHList
  where
    pOne :: Proxy (Succ Zero)
    pOne = Proxy
    pD :: Proxy (ToNat1 d)
    pD = Proxy










type family Dropped (k :: *) (xs :: [*]) where
  Dropped Z     xs        = xs
  Dropped k     '[]       = '[]
  Dropped (S k) (x ': xs) = Dropped k xs

class Drop (k :: *) where
  dropRec :: Proxy k -> Rec el f fields -> Rec el f (Dropped k fields)

instance Drop Z where
  dropRec = const id

instance Drop k => Drop (S k) where
  dropRec _ RNil     = RNil
  dropRec _ (_ :& r) = dropRec (Proxy :: Proxy k) r







vect :: Vec (ToPeano 3) Int
vect = V.mk3 1 2 3


hl :: HList '[Int,Int,Int]
hl = vecToHList $ vect

tr :: PlainTRec Int '[DField 1, DField 2, DField 3]
tr = hListToRec (Proxy :: Proxy (ToNat1 1)) (Proxy :: Proxy (ToNat1 3)) hl





class Split (xs :: [*]) (ys :: [*]) where
  splitRec :: Rec el f (xs ++ ys) -> (Rec el f xs, Rec el f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)

instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)




sp :: (PlainTRec Int '[DField 1], PlainTRec Int '["name" :~>: String])
sp = splitRec pt

--------------------------------------------------------------------------------
-- | Defining points in a d-dimensional space

data Point' (d :: Nat) (r :: *) (fields :: [*]) where
  Point' :: PlainTRec r (R d) -> PlainTRec r fields -> Point' d r fields


point' :: forall d r fields allFields.
          ( Split (R d) fields
          , allFields :~: (R d ++ fields)
          ) => PlainTRec r allFields -> Point' d r fields
point' = uncurry Point' . splitRec . cast





















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


-- _xL :: forall d r. (DField 1 ∈ R1 d) => Lens' (PlainRec (TElField r) (R1 d)) r
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


pt2 :: PlainRec (TElField Int) (R 2 ++ '["name" :~>: String])
pt2 = cast $ pt <+> y =: 5

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
