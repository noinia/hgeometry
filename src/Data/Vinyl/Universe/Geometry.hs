{-# LANGUAGE PolyKinds #-}     --- TODO: Why do we need this?
module Data.Vinyl.Universe.Geometry where



import Data.Vinyl
import Data.Vinyl.TyFun
import Data.Vinyl.Universe.Field((:::), ElField(..))



import GHC.TypeLits


-- | A type expressing a dimension. Note that something like 'D 2' expresses
-- *only* the second dimention (in whatever space we consider). Say the
-- two-dimensional plane is expressed by the type `R 2`.
data D (d :: Nat)
  -- essentially, we needed a wrapper type to get back from kind nat to kind *

--------------------------------------------------------------------------------
-- | An alternative Open Vinyl Universe

-- | Fields are indexed by either a type level natural number (a dimension), or
-- by a type level string. If it is a natural number, the type of the value
-- will be determined by the interpretation function. If it is a string, we can
-- specify the type ourselves.
data Field :: * -> * where
  NatField :: KnownNat d    => Field (D d)
  SymField :: KnownSymbol s => Field (s ::: t)

-- | Singletons for Field
data SField :: * -> * where
  SNatField :: KnownNat d    => SField (Field (D d))
  SSymField :: KnownSymbol s => SField (Field (s ::: t))

-- | The universe/interpretation of the fields. The universe is paramteterized
  -- over a type t and a TyFun. Th type t is used as a ``default'' type, for
  -- fields for which we did not specify the type.
data TElField :: * -> (TyFun * *) -> * where
  TElField :: TElField t el

type instance App (TElField r) (Field (D d))     = r
type instance App (TElField r) (Field (s ::: t)) = t

-- | Shorthand for naming dimention fields
type DField (d :: Nat)  = Field (D d)

-- | And a fancy name for the symbol fields
type (s :: Symbol) :~> (t :: *) = Field (s ::: t)

-- | Similar shorthands for the corresponding singletons
type SDField (d :: Nat)             = SField (DField d)
type SSField (s :: Symbol) (t :: *) = SField (s :~> t)


-- | Type signature for a PlainRec with TElField as the interpreter
type PlainTRec (r :: *) = PlainRec (TElField r)
