module Data.Geometry.Plane( Plane(..)
                          , basis1
                          , basis2

                          , fromPoints

                          , normal
                          ) where

import Control.Lens(Lens', (^.))


import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Vector

import Data.Vinyl
import Data.Vinyl.Universe.Geometry

import Data.Type.Nat
import Data.Type.List

import GHC.TypeLits


--------------------------------------------------------------------------------


data Plane (fs :: [*]) (r :: *) where
  Plane :: PlainTRec r ('[ "basis1" :~> Vector (ToNat1 3) r
                         , "basis2" :~> Vector (ToNat1 3) r
                         ] ++ fs) -> Plane fs r


----------------------------------------

basis1 = SSymField :: SSField "basis1" (Vector (ToNat1 3) r)
basis2 = SSymField :: SSField "basis2" (Vector (ToNat1 3) r)


_rec :: Lens' (Plane fs r) (PlainTRec r ('[ "basis1" :~> Vector (ToNat1 3) r
                                          , "basis2" :~> Vector (ToNat1 3) r
                                          ] ++ fs))
_rec f (Plane r) = fmap (\r' -> Plane r') (f r)


-- _basis1 :: Lens' (Plane fs r) (Vector (ToNat1 3) r)
-- _basis1 =

--------------------------------------------------------------------------------


fromPoints       :: Num r => Point 3 fs r -> Point 3 fs r -> Point 3 fs r -> Plane '[] r
fromPoints p q r = Plane $ basis1 =: (p .-. q)
                           <+>
                           basis2 =: (p .-. r)


normal :: Plane fs r -> Vector (ToNat1 3) r
normal = undefined
