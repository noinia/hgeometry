{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Geometry.Point where

import Control.Lens

-- import Data.TypeLevel.Common
import Data.Typeable
import Data.Vinyl hiding (Nat)
import qualified Data.Vinyl as V

-- import qualified Data.Vector.Fixed as FV
-- import qualified Data.Vector.Fixed.Cont as C

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude.Bool(If, (:&&))
import Data.Singletons.Prelude.List((:++))

import GHC.TypeLits


--------------------------------------------------------------------------------

data DField u = Coord Nat | DExt u


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
  Point { _unP :: Rec (DAttr d r elF) rs }
  deriving (Typeable)

instance (Show (Rec (DAttr d r elF) rs)) => Show (Point d elF rs r) where
  show (Point r) = "Point " ++ show r

instance (Eq (Rec (DAttr d r elF) rs)) => Eq (Point d elF rs r) where
  (Point l) == (Point r) = l == r

-- TODO: Maybe we want to explicitly lexicographically compare the coordinates
-- and only then the remaining fields.
instance (Ord (Rec (DAttr d r elF) rs)) => Ord (Point d elF rs r) where
  (Point l) `compare` (Point r) = l `compare` r

--------------------------------------------------------------------------------

-- | Proxy type to index the coordinate fields of a point
data C (n :: Nat) where C :: C n

--------------------------------------------------------------------------------

class Split (xs :: [k]) (ys :: [k]) where
  splitRec :: Rec f (xs :++ ys) -> (Rec f xs, Rec f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)
instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)






assign' :: (IsDimIndexOf d n ~ True) => sing n -> r -> DAttr d r elF (Coord n)
assign' _ x = DAttr x

c1 :: Num r => DAttr 1 r elF (Coord 1)
c1 = assign' (C :: C 1) 0

origin2 :: Num r => Point 2 elF '[Coord 1, Coord 2] r
origin2 = Point $  assign' (C :: C 1) 0
                :& assign' (C :: C 2) 0
                :& RNil




-- toVector           :: (Vector v r, Dim v ~ d)
--                    => Point d f rs -> v r
-- toVector (Point r) = vector $
