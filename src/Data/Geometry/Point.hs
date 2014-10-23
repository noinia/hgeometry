{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Geometry.Point where

import           Control.Lens(makeLenses)

-- import Data.TypeLevel.Common
import           Data.Typeable
import           Data.Vinyl hiding (Nat)
import qualified Data.Vinyl as V
import           Data.Vinyl.Functor(Const(..))
import qualified Data.Vinyl.TypeLevel as TV

import Data.Vinyl.TypeLevel hiding (Nat)

-- import qualified Data.Vector.Fixed as FV
-- import qualified Data.Vector.Fixed.Cont as C

import           Data.Geometry.Vector

import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.Prelude.Bool(If, (:&&))
import           Data.Singletons.Prelude.Base(IdSym0)
import           Data.Singletons.Prelude.List((:++))

import           GHC.TypeLits

import           Linear.Vector(Additive(..))
import           Linear.Affine hiding (Point(..))


--------------------------------------------------------------------------------

data DField u = Coord Nat | DExt u


data CoordSym0 :: TyFun Nat (DField u) -> *
type instance Apply CoordSym0 n = Coord n


type family IsDimIndexOf (d :: Nat) (n :: Nat) :: Bool where
  IsDimIndexOf d n = (1 <=? n) :&& (n <=? d)

type family DElF (d :: Nat)
                 (r :: *)
                 (elF :: TyFun u * -> *)
                 (fld :: DField u) where
  DElF d r elF (Coord n) = If (IsDimIndexOf d n) r Void
  DElF d r elF (DExt  r) = Apply elF r

-- data DElF_Sym3 :: (TyFun (Either Nat u) * -> *




-- data DElF_Sym2 :: (TyFun (TyFun u * -> *)
--                          (TyFun (Either V.Nat u) * -> *) -> * )



--------------------------------------------------------------------------------
newtype DAttr d r elF fld = DAttr { _unDAttr :: DElF d r elF fld }
                          deriving (Eq,Ord,Typeable)
makeLenses ''DAttr

instance (KnownNat n, Show r, IsDimIndexOf d n ~ True)
         => Show (DAttr d r elF (Coord n)) where
  show (DAttr x) = concat ["coord_"
                          , show . natVal $ (Proxy :: Proxy n)
                          , ": ", show x
                          ]

instance (Show fld, Show (DElF d r elF (DExt fld)))
         => Show (DAttr d r elF (DExt fld)) where
  show (DAttr x) = show (Proxy :: Proxy fld) ++ ": " ++ show x


--------------------------------------------------------------------------------

newtype Point (d :: Nat) (elF :: TyFun u * -> *) (rs :: [DField u]) (r :: *) =
  Point (Rec (DAttr d r elF) (DFields d ++ rs))
  deriving (Typeable,Show)

unP :: Point d elF rs r -> Rec (DAttr d r elF) (DFields d ++ rs)
unP (Point r) = r


instance (Eq (Rec (DAttr d r elF) (DFields d ++ rs))) => Eq (Point d elF rs r) where
  (Point l) == (Point r) = l == r

instance (Ord (Rec (DAttr d r elF) (DFields d ++ rs))) => Ord (Point d elF rs r) where
  (Point l) `compare` (Point r) = l `compare` r

--------------------------------------------------------------------------------

class Split (xs :: [k]) (ys :: [k]) where
  splitRec :: Rec f (xs ++ ys) -> (Rec f xs, Rec f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)
instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)


splitCore :: Split (DFields d) rs => Point d elF rs r
          -> ( Rec (DAttr d r elF) (DFields d)
             , Rec (DAttr d r elF) rs
             )
splitCore = splitCore' (Proxy :: Proxy rs) . unP


splitCore' :: Split (DFields d) rs
           => Proxy rs -> Rec (DAttr d r elF) ((DFields d) ++ rs)
           -> ( Rec (DAttr d r elF) (DFields d)
              , Rec (DAttr d r elF) rs
              )
splitCore' _ = splitRec


class PointVecIso (k :: TV.Nat) where
  toVRec :: ( (s + FromVNat k) ~ d
            )
         => Proxy s -> Proxy k
         -> Rec (DAttr d r elF) (Range1 CoordSym0 s k)
         -> Rec (Const r)       (Range1 IdSym0    s k)

instance PointVecIso TV.Z where
  toVRec _ _ _ = RNil



instance PointVecIso k => PointVecIso (TV.S k) where
  -- toVRec (Proxy :: Proxy s) (Proxy :: Proxy (TV.S k))
  --        (r :& rs) = r' :& toVRec (Proxy :: Proxy (1+s)) (Proxy :: Proxy k) rs
  --   where
  --     r' = undefined






-- class PointToVec (k :: TV.Nat) where
--   dRecToVec :: Proxy s -> Proxy k -> Rec (DAttr d r elF) (Range1 CoordSym0 s k)
--                -> Rec (Const r) (Range1 IdSym0 s k)


-- instance PointToVec k => PointToVec (TV.S k) where
--   dRecToVec (Proxy :: Proxy s) _ ((DAttr x) :& rs) =
--     Const x :& dRecToVec (Proxy :: Proxy (1+s)) (Proxy :: Proxy k) rs


type DFields (d :: Nat) = Range1 CoordSym0 1 (ToVNat d)

-- convert :: forall d r elF. Rec (DAttr d r elF) (DFields d) -> Rec (Const r) (DFields d)
-- convert = rmap f
--   where
--     f :: forall x. DAttr d r elF (Coord x) -> Const r x
--     f = Const . _unDAttr



-- convert :: Rec (DAttr d r elF) (DFields d) -> Rec (Const r) (DimRange d)
-- convert RNil = RNil
-- convert (r :& rs) = f r :& convert rs
--   where
--     f :: forall d r elF. forall x. DAttr d r elF x -> Const r x
--     f = Const . _unDAttr



toVec' :: Rec (DAttr d r elF) (DFields d) -> Vector d r
toVec' = undefined

toVec :: Point d elF rs r -> Vector d r
toVec = undefined

-- class VecToRec (pd :: TV.Nat) where
--   recToPoint :: ((s `InDimRange` d) ~ True, pd ~ ToNat d)
--              => Proxy s -> Proxy pd -> Rec (Const r) (Range1 )



--                 (Range1 IdSym0 (ToNat s) d)
--                 -> Rec (DAttr )


-- instance VecToRec TV.Z where
--   recToPoint _ _ _ = RNil
-- instance VecToRec k => VecToRec (TV.S k) where
--   recToPoint (Proxy :: Proxy s) psk ((Const x) :& rs) =
--     r :& recToPoint (Proxy :: Proxy (1+s)) (Proxy :: Proxy k) rs
--     where
--       r :: DAttr d r elF (Coord s)
--       r = DAttr x



toPoint :: Vector d r -> Point d elF '[] r
toPoint (Vector r) = Point $ undefined




assign' :: (IsDimIndexOf d n ~ True) => sing n -> r -> DAttr d r elF (Coord n)
assign' _ x = DAttr x

c1 :: Num r => DAttr 1 r elF (Coord 1)
c1 = assign' (C :: C 1) 0

origin2 :: Num r => Point 2 elF '[] r
origin2 = Point $  assign' (C :: C 1) 0
                :& assign' (C :: C 2) 0
                :& RNil

toPointRec :: Vector d r -> Rec (DAttr d r elF) (DFields d)
toPointRec = undefined

type AlwaysTrue d rs = ( Split (DFields d) rs
                       , RecApplicative (DimRange d)
                       )

instance AlwaysTrue d rs => Affine (Point d elF rs) where
  type Diff (Point d elF rs) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = let (pc,rest) = splitCore p
                pv        = toPointRec (toVec' pc ^+^ v)
            in Point $ pv <+> rest




--   (Point )


-- toVector           :: (Vector v r, Dim v ~ d)
--                    => Point d f rs -> v r
-- toVector (Point r) = vector $



-- class Additive (Diff p) => GeneralizedAffine p where
--   type Diff p

--   (.-.) :: (Num a, GeneralizedAffine q, Diff p ~ Diff q) => p a -> q a -> Diff p a
