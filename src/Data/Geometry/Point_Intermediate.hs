--- Intermediate steps for Vector To Point Conversion.




type family Replicate (n :: *) (t :: *) where
  Replicate Z     t = '[]
  Replicate (S n) t = (t ': Replicate n t)

type Replicate1 (n :: Nat1) (t :: *) = Replicate (Nat1ToPeano n) t


--------------------------------------------------------------------------------

class VecToHList (d :: *) where
  vecToHList :: Arity d => Vec d r -> HList (Replicate d r)

instance VecToHList Z where
  vecToHList _ = RNil

instance (Arity d, VecToHList d) => VecToHList (S d) where
  vecToHList v = let (x,xs) = (V.head v, V.tail v)
                 in (Identity x) :& vecToHList xs

----------------------------------------

class HListToPlainTRec (d :: Nat1) where
  hListToRec :: Proxy s -> Proxy d -> HList (Replicate1 d r) -> PlainTRec r (Range1 s d)


instance HListToPlainTRec Zero where
  hListToRec _ _ RNil = RNil
instance HListToPlainTRec d => HListToPlainTRec (Succ d) where
  hListToRec (_ :: Proxy s) _ (Identity r :& rs) = (Identity r)
                                                   :&
                                                   hListToRec (Proxy :: Proxy (Succ s))
                                                              (Proxy :: Proxy d) rs



vect :: Vec (ToPeano 3) Int
vect = V.mk3 1 2 3

hl :: HList '[Int,Int,Int]
hl = vecToHList $ vect

tr :: PlainTRec Int '[DField 1, DField 2, DField 3]
tr = hListToRec (Proxy :: Proxy (ToNat1 1)) (Proxy :: Proxy (ToNat1 3)) hl
