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


-- class BuildRec (k :: *) (d :: *)  where
--   build :: ( Index k (S d)
--            , KnownNat k'
--            , k' ~ ToNat (S k)
--            ) =>
--            Proxy k -> Vec (S d) r -> PlainTRec r (R k')

-- instance BuildRec Z d where
--   build _ _ = RNil

-- instance BuildRec (S k) d where
--   build _ v = (undefined :: PlainTRec r (R (ToNat k)))
--               <+>
--               (undefined :: PlainTRec r '[DField (ToNat (S k))])

--               -- (SNatField :: SDField (ToNat (S k))) =: V.index v (undefined :: S k)





-- = SNatField :: SDField (ToNat (S i))




-- indexVec :: forall i d r.
--             ( KnownNat (ToNat (S i))
--             , Index i d
--             , Arity d
--             )
--             => Vec d r -> PlainTRec r '[DField (ToNat (S i))]
-- indexVec v = (SNatField :: SDField (ToNat (S i))) =: V.index v (undefined :: i)

-- class FromVec (i :: *) where
--   vecToRec :: (Index i (S d), Arity d)
--            => Proxy i -> Vec (S d) r -> PlainTRec r '[DField (ToNat (S i))]

-- instance FromVec Z where
--   vecToRec _ v = (SNatField :: SDField 1) =: V.index v (undefined :: Z)

-- instance ( KnownNat (ToNat (S (S i)))
--          ) => FromVec (S i) where
--   vecToRec _ v = (SNatField :: SDField (ToNat (S (S i)))) =: V.index v (undefined :: S i)


-- class (Index i (S d), Arity d) => BuildVec (i :: *) (d :: *) where
--   build :: Proxy i -> Vec (S d) r -> PlainTRec r (R (ToNat (S i)))

-- instance (Arity d) => BuildVec Z d where
--   build p v = vecToRec p v

-- instance (Index i (S d), BuildVec i (S d)) => BuildVec (S i) (S d) where
--   build p v = cast $
--               undefined
--               :&
--               build (Proxy :: Proxy i) v

type family Replicate (n :: *) (t :: *) where
  Replicate Z     t = '[]
  Replicate (S n) t = (t ': Replicate n t)

type family (n :: *) :-: (m :: *) where
  Z :-: m = Z
  n :-: Z = Z
  (S n) :-: (S m) = n :-: m

infixl 6 :-:

class VecToHList (d :: *) where
  vecToHList :: Arity d => Vec d r -> HList (Replicate d r)

instance VecToHList Z where
  vecToHList _ = RNil

instance (Arity d, VecToHList d) => VecToHList (S d) where
  vecToHList v = let (x,xs) = (V.head v, V.tail v)
                 in (Identity x) :& vecToHList xs


class HListToPlainTRec (d :: *) where
  hListToRec :: HList (Replicate d r) -> PlainTRec r (R (ToNat d))

  -- dropOne :: PlainTRec r (R (ToNat d)) -> PlainTRec r (
  --                                           Dropped (S Z) (R (ToNat d)))

instance HListToPlainTRec Z where
  hListToRec RNil = RNil
  -- dropOne    RNil = RNil


-- instance HListToPlainTRec d => HListToPlainTRec (S d) where
--   hListToRec (r :& rs) = r :& (dropOne $ hListToRec rs)






--     where
--       f _ = undefined


--     (x :& xs) = case pk of
-- --     -- (Proxy :: Proxy Z)     -> x :& hListToRec pd (Proxy :: Proxy z) xs
-- --     (Proxy :: Proxy (S k)) -> hListToRec pd (Proxy :: Proxy k) xs











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





-- fromProxy :: Proxy k -> SDField (ToNat k)
-- fromProxy


vecToRec = undefined

-- instance ( KnownNat (ToNat (S (S i)))
--          , Arity d
--          , BuildVec i (S d)
--          , Index i (S d)
--          ) => BuildVec (S i) (S d) where
--   build p v = build (Proxy :: Proxy i) v
--               <+>
--               vecToRec p v

  -- build p v = build (Proxy :: Proxy i) v
  --             <+>
  --             vecToRec p v




-- class FromVec Index i (Len rs) => (i :: *) (rs :: [*]) where
--   vecToRec :: Vec d r -> PlainTRec r rs

-- -- instance FromVec '[] where
-- --   vecToRec _ = RNil

-- -- instance (KnownNat i, FromVec rs) => FromVec (DField i ': rs) where
-- --   vecToRec v = (SNatField :: SDField i) =: V.index v (undefined :: ToPeano (i - 1))
-- --                <+>
-- --                vecToRec v


vect :: Vec (ToPeano 3) Int
vect = V.mk3 1 2 3

tr :: PlainTRec Int '[DField 1]
tr = vecToRec (Proxy :: Proxy Z) vect

-- ix = V.index v (undefined :: ToPeano 1)

-- -- | The nats from fixed-vector don't have a common kind :(
-- class Index k d => FromVec (k :: *) (d :: *) where
--   vecToRec :: Vec d r -> PlainTRec r (R (ToNat k))

-- instance Arity d => FromVec Z (S d) where
--   vecToRec _ = RNil

-- instance (Arity d, FromVec k (S d), Index k d
--          ) => FromVec (S k) (S d) where
--   vecToRec v =



--     let field = SNatField :: SDField (ToNat (S k))
--                in  vecToRec v
--                    <+>
--                    field =: (V.index v (undefined :: k))


-- -- instance FromVec (S d) where
-- --   fromVec v = fromVec



-- constructRec     :: forall d k r. Index k d
--                  => Proxy k -> Vec d r -> PlainTRec r (R (ToNat k))
-- constructRec _ v = constructRec ... v
--                    <+>



-- vecToRec :: forall d r. Vec d r -> PlainTRec r (R (ToNat d))
-- vecToRec v = V.foldr

-- withFields :: Vec d r -> Vec d (Pla)


    -- fromVec

    --


--                   (Point' g _) = fromVec xs
--               in Point' ()



-- fromVec


-- class IsoRLen

--       (d :: Nat) (n :: *)




-- toContVec :: Proxy d -> Proxy k ->
--               PlainTRec r (Dropped (ToNat1 k) (R d)) -> ContVec (ToPeano (d - k)) r
-- toContVec _ _ RNil      = empty


-- toContVec _ (r :& rs) = undefined -- unIdentity r <| toContVec (Proxy :: Proxy (d - 1)) rs





-- extraFields :: forall r d d1 fields. (d1 ~ ToNat1 d, Drop d1)
--             => PlainRec (TElField r) (R d ++ fields)
--             -> PlainRec (TElField r) (Dropped d1 (R d ++ fields))
-- extraFields = dropRec (Proxy :: Proxy d1)



-- split :: forall el f xs ys. Rec el f (xs ++ ys) -> (Rec el f xs, Rec el f ys)
-- split rs = (cast rs, cast rs)


class Split (xs :: [*]) (ys :: [*]) where
  splitRec :: Rec el f (xs ++ ys) -> (Rec el f xs, Rec el f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)

instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)
  -- sndRec (_ :& rs) = sndRec rs

-- sndRec' :: (r ~ x, rs ~ (xs ++ ys), Split xs ys) =>
--            Rec el f (r ': rs) -> Rec el f ys
-- sndRec' = undefined





sp :: (PlainTRec Int '[DField 1], PlainTRec Int '["name" :~>: String])
sp = splitRec pt

-- extraFields    :: Split xs rs =>
--                   Proxy xs
--                -> PlainRec (TElField r) (xs ++ rs)
--                -> PlainRec (TElField r) rs
-- extraFields _ = snd . splitRec

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
