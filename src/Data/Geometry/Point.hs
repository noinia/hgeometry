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
import Data.Geometry.Vector

import Data.Vector.Fixed.Cont(Z(..),S(..),ContVec(..),ToPeano(..))
import Data.Vector.Fixed((<|),Arity(..))
import Data.Vector.Fixed.Boxed(Vec)


import Data.Vinyl
import Data.Vinyl.Idiom.Identity
import Data.Vinyl.TyFun
import Data.Vinyl.Lens


import Data.Vinyl.Universe.Geometry
import Data.Vinyl.Show

import Data.Type.Nat
import Data.Type.Equality hiding ((:~:))

import GHC.TypeLits

import qualified Data.Vector.Fixed as V

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Points in a d-dimensional space

-- | A Point in a d dimensional space. Apart from coordinates in R^d may have
-- additonal fields/attributes. For example color, a label, etc.
data Point (d :: Nat) (fs :: [*]) (r :: *) where
  Point :: PlainTRec r (R d) -> PlainTRec r fs -> Point d fs r


-- | Smart constructor that allows a different order of the input fields
point :: forall d r fields allFields.
          ( Split (R d) fields
          , allFields :~: (R d ++ fields)
          ) => PlainTRec r allFields -> Point d fields r
point = uncurry Point . splitRec . cast


----------------------------------------
-- Associated types

type instance Dimension (Point d fs r) = d

type instance NumType   (Point d fs r) = r

----------------------------------------
-- Instances

instance (Eq (PlainTRec r (R d)), Eq (PlainTRec r fs)) => Eq (Point d fs r) where
  (Point a b) == (Point c d) = a == c && b == d

instance (Ord (PlainTRec r (R d)), Ord (PlainTRec r fs)) => Ord (Point d fs r) where
  compare (Point a b) (Point c d) = case compare a c of
                                     EQ -> compare b d
                                     r  -> r

instance ( ChangeInterpretation fs
         , Functor' (Succ Zero) (ToNat1 d)
         ) => Functor (Point d fs) where
  fmap f (Point g rs) = Point (fmap'' (Proxy :: Proxy d) f g) (changeInt rs)


instance ( RecAll (TElField r) Identity (R d)  Show -- The (R d) part is showable
         , RecAll (TElField r) Identity fs Show -- The fs part is showable
         , Implicit (StringRec (R d))
         , Implicit (StringRec fs)
         ) => Show (Point d fs r) where
  show (Point g rs) = concat [ "Point "
                             , rshow g
                             , rshow rs
                             ]

--------------------------------------------------------------------------------
-- | A defintition of a d dimentional space

-- | R d is a type level list containing all DFields for dimensions 1 t/m d
-- type R (d :: Nat) = R1 (ToNat1 d)
type R (d :: Nat) = Range 1 d

type R1 (d :: Nat1) = Range1 (Succ Zero) d

-- | Type level list concatenation
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys       = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

infixr 5 ++


type Range (s :: Nat) (k :: Nat) = Range1 (ToNat1 s) (ToNat1 k)


type family Range1 (s :: Nat1) (k :: Nat1) where
  Range1 s Zero     = '[]
  Range1 s (Succ k) = DField (FromNat1 s) ': Range1 (Succ s) k

--------------------------------------------------------------------------------
-- | Conversion from Point to Vector

type family Len (xs :: [*]) where
  Len '[]       = Z
  Len (x ': xs) = S (Len xs)

class RecToContVec (rs :: [*]) where
  toContVec :: PlainTRec r rs -> ContVec (Len rs) r

instance RecToContVec '[] where
  toContVec _ = V.empty

instance RecToContVec rs => RecToContVec (DField i ': rs) where
  toContVec (r :& rs) = runIdentity r <| toContVec rs


toVec              :: forall d r fs. ( Len (R d) ~ ToPeano d
                                     , RecToContVec (R d)
                                     , Arity (ToPeano d)
                                     ) =>
                      Point d fs r -> Vec (ToPeano d) r
toVec (Point g _) = V.vector $ toContVec g


toVector :: forall d r fs. ( Len (R d) ~ ToPeano d
                           , ToPeano d ~ Nat1ToPeano (ToNat1 d)
                           , RecToContVec (R d)
                           , Arity (ToPeano d)
                           ) =>
            Point d fs r -> Vector (ToNat1 d) r
toVector = Vector . toVec




myVec2 = toVector myPt2


myPt2' = toPoint $ toVector myPt2


--------------------------------------------------------------------------------
-- | Conversion from Vector to Point

class VecToRec (d :: Nat1) where
  vecToRec :: Proxy s -> Proxy d -> Vector d r -> PlainTRec r (Range1 s d)

instance VecToRec Zero where
  vecToRec _ _ _ = RNil

instance (Arity (Nat1ToPeano d), VecToRec d) => VecToRec (Succ d) where
  vecToRec (_ :: Proxy s) _ v = let (x,xs) = destruct v in
                                (Identity x)
                                :&
                                vecToRec (Proxy :: Proxy (Succ s)) (Proxy :: Proxy d) xs

-- | Version of vecToRec without the proxies
vecToRec' :: forall d d1 r. ( VecToRec d1
                            , FromNat1 d1 ~ d, ToNat1 d ~ d1
                            )
                            => Vector d1 r -> PlainTRec r (R d)
vecToRec' = vecToRec (Proxy :: Proxy (ToNat1 1)) (Proxy :: Proxy d1)

----------------------------------------

-- | Convert a vector into a point
toPoint   :: forall d1 d r. ( VecToRec d1
                            , FromNat1 d1 ~ d, ToNat1 d ~ d1
                            )
                            => Vector d1 r -> Point d '[] r
toPoint = flip Point RNil . vecToRec'



myVect :: Vector (ToNat1 3) Int
myVect = Vector vect


-- vect :: Vec (ToPeano 3) Int
vect = V.mk3 1 2 3

myXX = toPoint myVect

--------------------------------------------------------------------------------
-- | Constructing a point from a monolithic PlainTRec


class Split (xs :: [*]) (ys :: [*]) where
  splitRec :: Rec el f (xs ++ ys) -> (Rec el f xs, Rec el f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)

instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)


sp :: (PlainTRec Int '[DField 1], PlainTRec Int '["name" :~> String])
sp = splitRec pt


--------------------------------------------------------------------------------
-- | Some common fields

-- | Some hands for the axis (fields) in the first three dimensions
x = SNatField :: SDField 1
y = SNatField :: SDField 2
z = SNatField :: SDField 3

-- | And a regular named field
name :: SSField "name" String --SField (Field ("name" ::: String))
name = SSymField


--------------------------------------------------------------------------------
-- | Lenses

-- | Syntactic sugar
type (n :: Nat) :<= (m :: Nat) = DField n ∈ R m








-- | Lens that gets the Rec containing only the geometric information for this point
_plainPoint                :: Lens' (Point d fs r) (PlainTRec r (R d))
_plainPoint f (Point g rs) = fmap (\g' -> Point g' rs) (f g)

-- | Non type-changing lens for the extra fields  of a point
_extraFields :: Lens' (Point d fs r) (PlainTRec r fs)
_extraFields f (Point g rs) = fmap (\rs' -> Point g rs') (f rs)


-- | Lens to get an axis based on its name
_axis     :: forall i d r fs. (i :<= d) => SDField i -> Lens' (Point d fs r) r
_axis fld = _plainPoint . _axisLens' (Proxy :: Proxy d) fld

-- | Lens to get an axis based on it's dimension/number
_axis'   ::  forall i d r fs. (i :<= d, KnownNat i) => Proxy i -> Lens'(Point d fs r) r
_axis' _ = _axis (SNatField :: SDField i)


-- | Lens to one of the extra fields of a point, based on the name of the field
_get     :: ((sy :~> t) ∈ fs) => SSField sy t -> Lens' (Point d fs r) t
_get fld = _extraFields . rLens' fld . _id


-- | Lens to one of the extra fields of a point, based on its name as a type
-- level string
_get'   :: forall sy t fs d r. ((sy :~> t) ∈ fs, KnownSymbol sy)
        => Proxy sy -> Lens' (Point d fs r) t
_get' _ = _get (SSymField :: SSField sy t)



_axisLens'       :: (i :<= d) => Proxy d -> SDField i -> Lens' (PlainTRec r (R d)) r
_axisLens' _ fld = rLens' fld . _id

_id :: Lens' (Identity x) x
_id f (Identity x) = fmap (\x' -> Identity x') (f x)



--------------------------------------------------------------------------------



test :: PlainTRec Int (R 2)
test = x =: 10 <+> y =: 5


pt :: PlainTRec Int [DField 1, "name" :~> String]
pt =   x    =: 10
   <+> name =: "frank"

pt2 :: PlainTRec Int (R 2 ++ '["name" :~> String])
pt2 = cast $ pt <+> y =: 5

myPt2 :: Point 2 '["name" :~> String] Int
myPt2 = point pt2

myPt :: Point 1 '["name" :~> String] Int
myPt = point pt


myName :: Lens' (PlainTRec Int [DField 1, "name" :~> String]) String
myName = rLens' name . _id

myX :: forall rs d. (DField 1 ∈ R d) => Lens' (Point d rs Int) Int
myX = _axis x


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













--------------------------------------------------------------------------------
-- | Implementation of Functor instance for Point

newtype ConstRec (s :: Nat1) (d :: Nat1) (a :: *) = ConstRec (PlainTRec a (Range1 s d))

instance Functor' s d => Functor (ConstRec s d) where
  fmap f cr = fmap' (Proxy :: Proxy s) (Proxy :: Proxy d) f cr

-- | Implement Functor for ConstRec
class Functor' (s :: Nat1) (d :: Nat1) where
  fmap' :: Proxy s -> Proxy d -> (a -> b) -> ConstRec s d a -> ConstRec s d b

instance Functor' s Zero where
  fmap' _ _ _ _ = ConstRec RNil

instance Functor' (Succ s) d => Functor' s (Succ d) where
  fmap' ps pd f rss = ConstRec (r' :& rs')
    where
      (r,ps',pd',rs) = destructR ps pd rss
      r'             = Identity $ f r
      ConstRec rs' = fmap' ps' pd' f rs

destructR    :: Proxy s -> Proxy (Succ d)
             -> ConstRec s (Succ d) a
             -> (a, Proxy (Succ s), Proxy d, ConstRec (Succ s) d a)
destructR _ _ (ConstRec ((Identity r) :& rs)) = (r, Proxy, Proxy, ConstRec rs)


-- | fmap for the geometric part of our points
fmap''       :: forall a b d. Functor' (Succ Zero) (ToNat1 d)
             => Proxy d ->  (a -> b) -> PlainTRec a (R d) -> PlainTRec b (R d)
fmap'' _ f r = r'
  where
    ConstRec r' = fmap' (Proxy :: Proxy (Succ Zero)) (Proxy :: Proxy (ToNat1 d)) f $ ConstRec r


-- | Class that expresses that we can change the interpretation of the type function. I.e.
-- our nonrelated fields have a type that has nothing to do with the
class ChangeInterpretation (rs :: [*]) where
  changeInt :: PlainTRec a rs -> PlainTRec b rs

instance ChangeInterpretation '[] where
  changeInt _ = RNil
instance ChangeInterpretation rs => ChangeInterpretation ((sy :~> t) ': rs) where
  changeInt (r :& rs) = r :& changeInt rs



--------------------------------------------------------------------------------
-- Some attempt at an defintion that also changes the types in the extra Fields


-- type family CheckFamily (h :: *) (a :: *) (b :: *) :: * where
--   CheckFamily a a b = b
--   CheckFamily h a b = h


-- class FunctorExtra (rs :: [*]) (a :: *) (b :: *) where
--   type OutputListType rs a b :: [*]
--   fmapExtra :: (a -> b) -> PlainTRec a rs -> PlainTRec b (OutputListType rs a b)


-- instance FunctorExtra '[] a b where
--   type OutputListType '[] a b = '[]
--   fmapExtra _ _ = RNil

-- instance FunctorExtra rs a b => FunctorExtra ((sy :~> a) ': rs) a b where
--   type OutputListType ((sy :~> a) ': rs) a b = (sy :~> b) ': OutputListType rs a b

--   fmapExtra f ((Identity r) :& rs) = Identity (f r) :& fmapExtra f rs

-- instance FunctorExtra rs a b => FunctorExtra ((sy :~> c) ': rs) a b where
--   type OutputListType ((sy :~> c) ': rs) a b = (sy :~> c) ': OutputListType rs a b

--   fmapExtra f (r :& rs) = r :& fmapExtra f rs


-- instance FunctorExtra rs a b => FunctorExtra ((sy :~> c) ': rs) a b where
--   type OutputListType ((sy :~> c) ': rs) a b = CheckFamily c a b ': OutputListType rs a b

--   fmapExtra f (r :& rs) = fmapExtraHead f r (Proxy :: Proxy c) (Proxy :: Proxy a) :& fmapExtra f rs


-- fmapExtraHead           :: (a -> b) -> c -> Proxy c -> Proxy a -> CheckFamily c a b
-- fmapExtraHead f x pc pa = case testEquality pc pa of
--   Just Refl -> f x
--   Nothing   -> x
